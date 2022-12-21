{-|
Module      : IconExtras
Description : Utilities for window Icon generation

This module provides a pure haskell implementation for generating XPM icons from a Window's _NET_WINDOW_ICON. 
This is useful for xmobar users since its real icon support is limited.
Icons are scaled using the haskell-image-processing (hip) library.
-}
module IconExtras (writeWindowIcon,iconCleanupHook) where

import Foreign.C (CUShort (CUShort), CUInt (CUInt))
import XMonad
import XMonad.Prelude (fi, All (All)) -- convenience; there's lots of C Types and Word types around
import XMonad.Util.WindowProperties (getProp32)
import Control.Monad.State ( unless, when, evalState, State )
import Text.Printf ( printf )
import Data.Bits ( Bits(shiftL, (.&.), shiftR) )
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric (showIntAtBase)
import Graphics.Image (Pixel(PixelRGBA))
import qualified Graphics.Image as I
import System.Directory (doesFileExist, removeFile, getXdgDirectory, XdgDirectory (XdgCache), createDirectoryIfMissing)

{-
$usage
This module requires the haskell-image-processing library which is not provided by xmonad-contrib. 
Adding dependencies varies by setup, but for a typical Stack setup, you would need to add `hip` to package.yaml.

To use this module with xmobar, you will need a custom Logger since the required IO isn't available to the formatters.
An example based on X.M.Loggers (logTitlesOnScreen):

> miniLogTitlesOnScreen' :: ScreenId -> (String -> String) -> (String -> String) -> Logger
> miniLogTitlesOnScreen' sid formatFoc formatUnfoc = (`withScreen` sid) $ \screen -> do
>  let focWin = fmap W.focus . W.stack . W.workspace $ screen
>      wins   = maybe [] W.integrate . W.stack . W.workspace $ screen
>  winNames <- traverse (fmap show . getName) wins
>  winIcons <- mapM (writeWindowIcon (16,16)) wins
>  let formatWindow :: Window -> String -> Maybe String -> String
>      formatWindow w n mi = maybe "" (printf "<icon=%s/>") mi ++ 
>        (if Just w == focWin then formatFoc n else formatUnfoc n)
>  pure . Just
>       . unwords
>       $ zipWith3 formatWindow wins winNames winIcons

Additionally, the provided Event hook should be used to clean up the icon files generated by this module. 

> handleEventHook = otherHooks <+> iconCleanupHook

-}

-- Toggle for trace logging
debug :: Bool
debug = False

-- Get/create the base directory for icons. Uses the XDG_CACHE path
initBaseDirectory :: IO FilePath
initBaseDirectory = do
  path <- getXdgDirectory XdgCache "xmonad-icons"
  createDirectoryIfMissing False path
  return path

-- |An Event Hook to delete an icon file when a Window is closed
iconCleanupHook :: Event -> X All
iconCleanupHook DestroyWindowEvent { ev_window = w } = do
  baseDir <- io initBaseDirectory
  let filepath = printf "%s/%d.xpm" baseDir (fi w :: Integer)
  exists <- io $ doesFileExist filepath
  when exists $ io $ do
    when debug $ trace $ "Removing file " ++ filepath
    removeFile filepath
  return (All True)
iconCleanupHook _ = return (All True)

-- Take a stream of ARGB data (from _NET_WM_ICON) and make an image using HIP
imageFromRaw :: Int -> [ARGB] -> I.Image I.VS I.RGBA I.Word8
imageFromRaw width xs = I.fromListsR I.VS $ group width pixels
  where pixels = map (\(a,r,g,b) -> PixelRGBA (fi r) (fi g) (fi b) (fi a)) xs

-- Resize/scale an image to fit xmobar
shrink :: (Int,Int) -> I.Image I.VS I.RGBA I.Word8 -> I.Image I.VS I.RGBA I.Word8
shrink = I.resize I.Nearest I.Edge

-- Reverse operation to get ARGB data back out of the HIP image
imageToRaw :: I.Image I.VS I.RGBA I.Word8 -> [[ARGB]]
imageToRaw img = (map . map) pixelToRGB $ I.toLists img
  where pixelToRGB (PixelRGBA r g b a) = (CUShort (fi a),CUInt (fi r),CUInt (fi g), CUInt (fi b))

-- |Retrieve, scale, and write a Window's icon to an XPM file.
writeWindowIcon :: (Int,Int)          -- ^ The desired target dimensions to scale the icon
                -> Window             -- ^ The Window to retrieve the icon data from
                -> X (Maybe FilePath) -- ^ The FilePath that was written, or Nothing if there was no icon
writeWindowIcon (targetWidth, targetHeight) w = do
  baseDir <- io initBaseDirectory
  let filepath = printf "%s/%d.xpm" baseDir (fi w :: Integer)
  alreadyExists <- io $ doesFileExist filepath
  if not alreadyExists then do
    wm_icon <- getAtom "_NET_WM_ICON"
    raw <- fromMaybe [] <$> getProp32 wm_icon w
    -- Assume if there's data, it's valid
    if length raw > 2 then do
      let asints = fi <$> raw :: [CUInt]
          [width,height] = take 2 asints
          -- There could be multiple icons the xprop. Just pick the first one for now
          rawPixels = take (fi width * fi height) $ drop 2 asints 
          rgbaPixels = parseRGBA <$> rawPixels
          img = shrink (targetWidth, targetHeight) $ imageFromRaw (fi width) rgbaPixels
          scaled = imageToRaw img
          xpm = makeXPM targetWidth targetHeight scaled
      when debug $ trace $ "Writing file " ++ filepath
      io $ writeFile filepath (unlines $ xpmToLines xpm)
      --io $ I.displayImage img -- Useful for debugging. Shows the image as a TIF
      return $ Just filepath
    else return Nothing
  else return $ Just filepath

-- Grab the low byte from CUInt
lowbyte :: CUInt -> CUInt
lowbyte i = i .&. 0x000000FF

-- Parse/convert the raw ARGB to an ARGB tuple
parseRGBA :: CUInt -> ARGB
parseRGBA raw = (a,r,g,b)
  where b = lowbyte raw
        g = lowbyte (shiftR raw 8)
        r = lowbyte (shiftR raw 16)
        a = fi $ lowbyte (shiftR raw 24)
-- Types
-- ARGB tuple, in that order
type ARGB = (CUShort, CUInt, CUInt, CUInt)
-- RGB tuple, in that order
type RGB  = (CUInt, CUInt, CUInt)
-- The XPM contents object
data XPM = XPM { _height :: Int
               , _width :: Int
               , _nDigits :: Int
               , _colors :: [String]
               , _pixels :: [String]
               , _cache :: XPMResolvedCache } deriving Show

-- Assemble an XPM file's contents
makeXPM :: Int -> Int -> [[ARGB]] -> XPM
makeXPM w h pix = XPM { _width = w
                      , _height = h
                      , _nDigits = n
                      , _colors = xpmColors
                      , _pixels = pixValues
                      , _cache = assignment }
  where xpmColors = transparentColor : Map.foldrWithKey (\rgb cp acc -> encodeColor rgb cp : acc) [] assignment
        pixValues = map (mapPixelsToCodepoints assignment transparent) pix
        encodeColor :: RGB -> String -> String
        encodeColor rgb codepoint = printf "\"%s c #%06X\"," codepoint (fi $ rgbToHex rgb :: Integer)
        (rawAssignment, lastValue) = evalState (parse $ concat pix) initialState
        (assignment, n) = createCodePoints rawAssignment lastValue
        transparent = replicate n identityCodepoint -- codepoint for transparent
        transparentColor = printf "\"%s c None\"," transparent

-- Lookup an ARGB value from the resolved cache of color code points, or default to transparent
mapPixelsToCodepoints :: XPMResolvedCache -> String -> [ARGB] -> String
mapPixelsToCodepoints cps trans row = printf "\"%s\"" $ concatMap bar row
  where bar :: ARGB -> String
        bar (_,r,g,b) =  Map.findWithDefault trans (r,g,b) cps

-- Simple RGB tuple assembled back into a CUInt
rgbToHex :: RGB -> CUInt
rgbToHex (r,g,b) = b + g' + r'
  where g' = shiftL g 8
        r' = shiftL r 16

-- Chunk pixels into width
-- Copied from SO
group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative or zero n"

-- Create actual codepoints for valid colors
-- An easy way to create codepoints is base conversion
-- This encodes Ints into base 57 using the ASCII range 'A' to 'z'
-- Reminder that XPM codepoints must all have the same length
createCodePoints :: XPMCache -> Int -> (XPMResolvedCache, Int)
createCodePoints cache maxValue = (Map.map (leftPad . encInt) cache, nDigits)
  where encInt i = showIntAtBase 57 intToDigit i ""
        intToDigit i = [ 'A' .. 'z' ] !! i  -- probably not efficient
        nDigits = length $ encInt maxValue
        leftPad s = replicate (nDigits - length s) identityCodepoint ++ s

-- The zero of our base 57 encoding
identityCodepoint :: Char
identityCodepoint = 'A'

-- The cache of RGB -> Resolved color codepoint
type XPMResolvedCache = Map RGB String
-- The raw cache of RGB -> intermediary codepoint identifiers
type XPMCache = Map RGB Int
-- State to collect the cache and last generated intermediary codepoint
type XPMState = (XPMCache, Int)

-- Given a set of pixels, make a Map of all unique, non-transparent RGB tuples
-- Each tuple is given a unique identifier as a value
-- Transparency classification is any ARGB value with more than 50% opacity
parse :: [ARGB] -> State XPMState XPMState
parse [] = get
parse ((a,r,g,b):xs) = do
    -- If a pixel is transparent, don't bother storing it. We will default to transparent later
    let isTransparent = (fi a :: Double) <= 255 / 2
    unless isTransparent $ do
        (cache, n) <- get
        let seen = Map.member (r,g,b) cache
            nextValue = if not seen then succ n else n
            nextCache = if seen then cache else Map.insert (r,g,b) nextValue cache
        put (nextCache, nextValue)
    parse xs

-- Start at 1 since 0 is reserved for transparency codepoint
initialState :: XPMState
initialState = (Map.empty, 1)

-- Piece together an XPMV3 Template
xpmToLines :: XPM -> [String]
xpmToLines xpm = [ "/* XPM */"
                   , "static char * XFACE[] = {"
                   , printf "\"%d %d %d %d\"," (_width xpm) (_height xpm) (length $ _colors xpm) (_nDigits xpm) ] ++
                   _colors xpm ++
                   [ intercalate ",\n" $ _pixels xpm
                   , "};" ]