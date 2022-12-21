{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module QTileColumns (
   QTileCol
  ,qtileCol
  ,NewCol(..)
) where

import XMonad
import qualified XMonad.StackSet as W
import Data.Map (Map)
import qualified Data.Map as Map

import XMonad.Prelude

-- Enable/disable trace messages
traceOn :: Bool
traceOn = True

data NewCol = ColLeft | ColRight
instance Message NewCol

qtileCol :: QTileCol a
qtileCol = QTileCol [1] Map.empty

data QTileCol a = QTileCol 
  { colCounts :: ![Int]
  , winToCol  :: !(Map Window Int)
  } deriving (Show, Read, Eq)
  
instance LayoutClass QTileCol Window where
  doLayout l@QTileCol{colCounts = cols, winToCol = known} r s = do 
      let 
          wins = W.integrate s
          missing = Map.keys known \\  wins -- windows we knew about that no longer exist
          --new = wins \\ Map.keys known
          toAdjust = mapMaybe (`Map.lookup` known) missing
          adjustedCounts = subtractMissing toAdjust cols
          model = createModel wins adjustedCounts known
          ncc = map length model
          newColCounts = if null ncc then [1] else ncc
          newMap = foldl' (\(m,i) col -> (Map.union m (mapFromCol col i),i+1)) (Map.empty,0) model
          mapFromCol col idx = foldl' (\m e -> Map.insert e idx m) Map.empty col
          nextLayout = l { colCounts = newColCounts, winToCol = fst newMap}
          rects = doL newColCounts r
          zipped = zip wins rects
      when traceOn $ trace $ intercalate "\n" [
           "doLayout: layout=" ++ show l
          ,"model=" ++ show model
          ,"stack=" ++ show wins
          ,"focused=" ++ show (W.focus s)
          ,"toAdjust=" ++ show toAdjust ++ "adjustedCounts=" ++ show adjustedCounts
          --,"rects=" ++ show rects
          ,"nextLayout=" ++ show nextLayout
          ]
      return (zipped, Just nextLayout)
  
  handleMessage l m = do
      let msg = fromMessage m :: Maybe NewCol
      x <- maybe (return l) (newCol l) msg
      if l == x then return Nothing else return $ Just x

  description _ = "QTileCol"
  

-- Remove missing windows from column counts (i.e. window was closed)
subtractMissing :: (Foldable t, Num a) => t Int -> [a] -> [a]
subtractMissing adj cols = foldr' subIdx cols adj
  where subIdx e acc = take e acc ++ subHead (drop e acc)
        subHead (x:xs) = (x-1):xs
        subHead [] = []

-- Create a new layout that has a new column to the left of the focused window
-- (Janky as it calls windows)
newCol :: QTileCol a -> NewCol -> X(QTileCol a)
newCol l ColLeft  = newCol' l $ helperL l
newCol l ColRight = newCol' l $ helperR l

newCol' :: QTileCol a -> (W.Stack Window -> X (QTileCol a)) -> X (QTileCol a)
newCol' l f = do
  ws <- gets windowset
  let s = W.stack . W.workspace . W.current $ ws
  newLayout <- maybe (return l) f s
  when traceOn $ trace $ intercalate "\n" 
    ["newCol:"
    ,"  layout=" ++ show l
    ,"  newLayout=" ++ show newLayout
    ]
  return newLayout

-- TODO Refactor combine these helpers
helperR :: QTileCol a -> W.Stack Window -> X (QTileCol a)
helperR l@QTileCol{colCounts = cols, winToCol = known} s = do
  let focused = W.focus s
      focusedCol = fromMaybe 0 $ Map.lookup focused known
      rightColEnd = sum $ take (focusedCol + 1) cols
      wins = reverse (W.up s) ++ W.down s
      (lefts,rights) = splitAt (rightColEnd-1) wins
      newStack = s { W.up = reverse lefts, W.down = rights }
      newLayout = l { colCounts = updateCounts' cols focusedCol (focusedCol+1) }
  windows (W.modify' $ const newStack)
  return newLayout

helperL :: QTileCol a -> W.Stack Window -> X(QTileCol a)
helperL l@QTileCol{colCounts = cols, winToCol = known} s = do
  let focused = W.focus s
      -- find column of focused (or 0 if we somehow don't know it)
      focusedCol = fromMaybe 0 $ Map.lookup focused known
      -- find the stack index for start of the left column
      leftColEnd = sum $ take focusedCol cols
      -- Make a new Stack by reordering things
      -- wins minus the focused since we'd filter it out anyway
      wins = reverse (W.up s) ++ W.down s
      (lefts,rights) = splitAt leftColEnd wins
      newStack = s { W.up = reverse lefts, W.down = rights }
      -- Update the column counts 
      newLayout = l { colCounts = updateCounts' cols focusedCol focusedCol }
  -- Update Stack state then return the new Layout
  windows (W.modify' $const newStack) -- probably a bad idea since this calls runLayout
  return newLayout

-- Decrement the active column by one
-- Sandwich a new column with one window into the counts
-- Filter out any columns that are zero
updateCounts' :: [Int] -> Int -> Int -> [Int]
updateCounts' cols activeCol c = filter (>0) $ lefts ++ [1] ++ rights
  where lefts  = take c decremented
        rights = drop c decremented
        decremented = take activeCol cols ++ decrement (drop activeCol cols)
        decrement (x:xs) = (x-1) : xs
        decrement []     = []

-- Organize the windows from the stack into columns based on previous knowledge of column counts
-- If a window is new, consider it as part of the column being evaluated
createModel :: [Window] -> [Int] -> Map Window Int -> [[Window]]
createModel wins cols cache = z
  where z = foldl' f [] cols
        f acc e = acc ++ (if null (thisCol e acc) then [] else [thisCol e acc])
        cursor l = sum $ fmap length l
        thisCol e acc = unsafeTakeSome e (drop (cursor acc) wins) cache
        

-- Jank. Take at least n if possible.  Unknown windows can make the length result > n
unsafeTakeSome :: (Ord a) => Int -> [a] -> Map a Int -> [a]
unsafeTakeSome _ [] _ = []
unsafeTakeSome 0 _  _ = []
unsafeTakeSome n (x:xs) m = x:unsafeTakeSome nextTake xs m
  where nextTake = if Map.member x m then n-1 else n
  

-- Actually lay out windows by the column counts
-- Copied from X.L.MultiColumns with equal spacing statically defined
doL :: [Int] -> Rectangle -> [Rectangle]
doL cols r = rlist
  where 
        s = -0.5 :: Rational
        ncol = length cols
        size = floor $ abs s * fromIntegral (rect_width r)
        width
          | s>0 = if ncol==1
                  -- Only one window
                  then [fromIntegral $ rect_width r]
                  -- Give the master it's space and split the rest equally for the other columns
                  else size:replicate (ncol-1) ((fromIntegral (rect_width r) - size) `div` (ncol-1))
          | fromIntegral ncol * abs s >= 1 = replicate ncol $ fromIntegral (rect_width r) `div` ncol
          | otherwise = (fromIntegral (rect_width r) - (ncol-1)*size):replicate (ncol-1) size
        -- Compute the horizontal position of columns
        xpos = accumEx (fromIntegral $ rect_x r) width
        -- Exclusive accumulation
        accumEx a (x:xs) = a:accumEx (a+x) xs
        accumEx _ _ = []
        -- Create a rectangle for each column
        cr = zipWith (\x w -> r { rect_x=fromIntegral x, rect_width=fromIntegral w }) xpos width
        -- Split the columns into the windows
        rlist = concat $ zipWith splitVertically cols cr
