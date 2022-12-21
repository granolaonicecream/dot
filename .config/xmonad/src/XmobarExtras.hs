{-# LANGUAGE TupleSections #-}
module XmobarExtras (
    toggleIconify,
    myXmobarPP
) where

import XMonad
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.List (stripPrefix, zipWith4)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar.PP (xmobarFont)
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.WindowProperties (getProp32)
import XMonad.Prelude (find,liftM)
import Colors
import IconExtras (writeWindowIcon)
import qualified XMonad.Util.ExtensibleState as XS  -- Custom State
import qualified XMonad.StackSet as W
import Text.Printf (printf)

-----------------------------------------------------------------------
-- Custom xmobar extensions, mostly around Logger
-- Features
--   * Minimize and Restore windows via scripts on mouse click
--   * Custom text prepended to window titles (e.g. icons)
--   * Toggle whether window titles or icons should be logged, globally
--   * Window titles logged per screen, not just active workspace
--   * Layout name to custom text (e.g. icons)
-----------------------------------------------------------------------

-----------------------------------------------------------------------
-- ExtensibleState to hold global setting for window titles
-- When Iconify True, window titles are shortened to their icon or first Char
data TitleState = Iconify Bool
instance ExtensionClass TitleState where
  initialValue = Iconify False

-- fmap for TitleState
apply :: (Bool -> Bool) -> TitleState -> TitleState
apply f (Iconify b) = Iconify (f b)

-- unwrap helper to reduce verbosity
unwrap :: TitleState -> Bool
unwrap (Iconify b) = b

-- Toggle whether or not to hide window titles in xmobar
toggleIconify :: X()
toggleIconify = do 
  XS.modify (apply not) 
  asks (logHook . config) >>= userCodeDef ()

-----------------------------------------------------------------------
-- Minimize/Restore script wiring.
-- Minimization can be done with xdotool.  However, xdotool cannot restore
--  without window activation, which is a weird experience.  Therefore, 
--  restoration is done by a custom C program to only change the WM_STATE.
--  Both actions rely on XMonad having the proper event hooks to listen for 
--  these XEvents.

-- Read an xprop to determine if a window is minimized
isWindowHidden :: Window -> X (Bool)
isWindowHidden w = withDisplay $ \dpy -> do
  wm_state <- getAtom "_NET_WM_STATE"
  hidden <- fromIntegral <$> getAtom "_NET_WM_STATE_HIDDEN"
  wstate <- fromMaybe [] <$> getProp32 wm_state w
  return (hidden `elem` wstate)

xmobarRestore :: Window -> String -> String
xmobarRestore w t = concat ["<action=`xmonadRestore ", show w, "`>", t, "</action>"]

xmobarMinimize :: Window -> String -> String
xmobarMinimize w t = concat ["<action=`xdotool windowminimize ", show w, "`>", t, "</action>"]


-----------------------------------------------------------------------
-- Custom window title Logger
-- Largely copied from XMonad.Util.Loggers but modified to allow extra 
--  decoration based on each window

-- | A shortcut to a screen
type WindowScreen = W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail

-- | A helper function to create screen-specific loggers.
withScreen :: (WindowScreen -> X (Maybe a)) -> ScreenId -> X (Maybe a)
withScreen f n = do
  ss <- withWindowSet $ return . W.screens
  case find ((== n) . W.screen) ss of
    Just s  -> f s
    Nothing -> pure Nothing

miniLogTitlesOnScreen' :: ScreenId -> (String -> String) -> (String -> String) -> Logger
miniLogTitlesOnScreen' sid formatFoc formatUnfoc = (`withScreen` sid) $ \screen -> do
 let focWin = fmap W.focus . W.stack . W.workspace $ screen
     wins   = maybe [] W.integrate . W.stack . W.workspace $ screen
 winNames <- traverse (fmap show . getName) wins
 winIcons <- mapM (writeWindowIcon (16,16)) wins
 let formatWindow :: Window -> String -> Maybe String -> String
     formatWindow w n mi = maybe "" (printf "<icon=%s/>") mi ++
       (if Just w == focWin then formatFoc n else formatUnfoc n)
 pure . Just
      . unwords
      $ zipWith3 formatWindow wins winNames winIcons


-- Like Xmonad.Util.Loggers but also styles the text based on:
--  * window class name
--  * ExtensibleState TitleState
--  * window minimization status
myLogTitlesOnScreen'
  :: ScreenId           -- ^ Screen to log the titles on
  -> (Maybe String -> String -> String) -- ^ Formatting for the focused   window
  -> (Maybe String -> String -> String) -- ^ Formatting for the unfocused window
  -> (Maybe String -> String -> String) -- ^ Formatting for the minimized window
  -> Logger
myLogTitlesOnScreen' sid formatFoc formatUnfoc formatMin = (`withScreen` sid) $ \screen -> do
  let focWin = fmap W.focus . W.stack . W.workspace $ screen
      wins   = maybe [] W.integrate . W.stack . W.workspace $ screen
  winNames <- traverse (fmap show . getName) wins
      --winNames = fmap show wins
  -- winClasses <- traverse (runQuery className) wins
  winHidden <- traverse isWindowHidden wins
  winIcons <- mapM (writeWindowIcon (16,16)) wins
  sw <- XS.get :: X TitleState
  let nameFmt = iconfiyWindowName $ unwrap sw
      decorated = map nameFmt winNames
      formatWindow :: Window -> String -> Bool -> Maybe String -> String
      formatWindow w n h mi = if h
        then xmobarRestore w $ formatMin mi n
        else xmobarMinimize w (if Just w == focWin then formatFoc mi n else formatUnfoc mi n)
  pure . Just
       . unwords
       $ zipWith4 formatWindow wins decorated winHidden winIcons

-- | Like 'logTitlesOnScreen', but directly use the "focused" screen
-- (the one with the currently focused workspace).
-- logTitles' :: (String -> String) -> (String -> String) -> Logger
-- logTitles' formatFoc formatUnfoc = do
--   sid <- gets $ W.screen . W.current . windowset
--   myLogTitlesOnScreen' sid formatFoc formatUnfoc formatUnfoc

-- Decorate the window name with an icon if it exists
-- If window titles should be iconified, use icon or first char
decorateWindowName :: Bool -> String -> String -> String
decorateWindowName iconify name className = fromMaybe noIcon $ liftM lifter icon
  where icon = classNameToIcon className
        firstChar [] = ""
        firstChar s  = [s!!0]
        noIcon = if iconify then firstChar name else name
        lifter = if iconify then (flip (++) " ") else (\i -> wrap i name "  ")

-- Known window glyph icons to prepend (Nerd Font codepoints)
classNameToIcon :: String -> Maybe String
classNameToIcon cn = case cn of
  "firefox" -> Just "\xf269"
  -- "firefox" -> Just "<icon=firefox.xpm/>"
  "Alacritty" -> Just "\xf120"
  "VSCodium" -> Just "\xf1c9"
  "discord" -> Just "\xfb6e"
  "Steam" -> Just "\xf1b6"
  "xdpager" -> Just "\xfa6f"
  _ -> Nothing

-- Shorten a String if true
iconfiyWindowName :: Bool -> String -> String
iconfiyWindowName True _ = ""
iconfiyWindowName False name = name

-----------------------------------------------------------------------
-- Color constants
--color1 = "#eac440" -- main highlight color (focus, active ws)

--color1 = "#fdd870" -- main highlight color (focus, active ws)
--color1 = "#f2e750"
--color2 = "#363636"
--color2 = "#351409" -- box background

--color3 = "#6b6b6b"
--color3 = "#d0902f" -- darkened text
--color4 = "#a15501" -- box border, visible workspace

-- xmobarColor but set the textOffset to 0
xmobarColor' fg bg = wrap t "</fc>"
    where t = concat ["<fc=", fg, ",", bg, ":0>"]

-----------------------------------------------------------------------
-- Put it all together with an xmobar pretty printer
myXmobarPP :: ScreenId -> PP
myXmobarPP sid = def
    { ppTitleSanitize = xmobarStrip
    , ppCurrent = focusedColor
    , ppVisible = xmobarColor' color3  color2
    , ppOrder = \[ws, _, _, wins, l] -> [workspaceBox ws, l, wins]
    , ppExtras = [myLogTitlesOnScreen' sid formatFocused formatUnfocused formatMinimized
    -- , ppExtras = [miniLogTitlesOnScreen' sid formatFocused formatUnfocused 
                 ,(fmap . fmap) layoutBox $ logLayoutOnScreen sid -- screen aware ppLayout
                 ]
    , ppSep = " "
    , ppRename = \n _ -> xmobarFont 2 n  -- pixelsize in Nerd Fonts aren't great

    }
  where
    injectIcon mi s = maybe s (\i -> printf " <icon=%s/>%s" i s) mi
    formatFocused mi = focusedColor . highlightBox . injectIcon mi . wrap " " " " . ppWindow
    formatUnfocused mi = unfocusedColor . injectIcon mi . wrap " " " " . ppWindow
    -- formatMinimized mi = xmobarColor' "#ec861e" "#724314" . injectIcon mi . wrap " " " " . ppWindow
    formatMinimized mi = xmobarColor' "#dddaa0" "#64523f" . injectIcon mi . wrap " " " " . ppWindow
--    formatFocused = focusedColor . highlightBox . wrap "\xe0bc " " " . ppWindow
--    formatUnfocused = unfocusedColor . normalBox . wrap "  " " " . ppWindow
--    formatMinimized = minimizedBox . xmobarColor' "grey" "#404040" . wrap "  " " " . ppWindow

    workspaceBox =  xmobarAction "~/scripts/pager-wrapper.sh" "2" . normalBox . xmobarColor' color4 color2 . wrap " " " "
    layoutBox = normalBox .  xmobarColor' color3 color2 . wrap " " " " . layoutToIcon

    ppWindow :: String -> String
    ppWindow = (\w -> if null w then "-" else w) . shorten' "-" 32
    -- ppWindow = (\w -> if null w then "---" else w) . shorten' "-" 32
    -- ppWindow = xmobarRaw . (\w -> if null w then "---" else w) . shorten' "-" 32

    normalBox = xmobarBorder "Full" color4 1
    highlightBox = xmobarBorder "Full" color1 1
--    minimizedBox = xmobarBorder "Full" "grey" 1
    focusedColor = xmobarColor' color1 color2
    -- unfocusedColor = xmobarColor' color3 color2
    unfocusedColor = xmobarColor' color3 color2

    -- Have to filter emoji since xmobar relies on XFT which can't do color emoji without a brga patch
    -- TODO: get more accurate emoji code range... things like hearts should also count
    isEmoji c = (c >= '\x1F600' && c <= '\x1F64F')
             || (c >= '\x1F300' && c <= '\x1F5FF')
             || (c >= '\x1F680' && c <= '\x1F6FF')
             || (c >= '\x1F1E0' && c <= '\x1F9FF')
             || (c == '\x2764' || c == '\x2714')



-- Alias for the first additional font, which is siji
xmobarSymbol :: String -> String
xmobarSymbol  = xmobarFont 1

-- Transform a layout name (probably renamed) into a glyph/icon
layoutToIcon :: String -> String
layoutToIcon l = concat [rAxis, rAxis2, magOn, helperLayoutToIcon l''']
    where (rAxis,l')   = stripReflect l
          (rAxis2,l'') = stripReflect l' -- do it twice to check for each Reflect axis
          (magOn,l''') = stripMagnify l''

-- Transform the description of Reflect into the axis if enabled
stripReflect :: String -> (String,String)
stripReflect l = maybe ("",l) (\(d:' ':suffix) -> ([d], suffix)) $ stripPrefix "Reflect" l

-- Transform the description of Magnifier into a glpyh if enabled
stripMagnify :: String -> (String, String)
stripMagnify l = maybe def ("",) (mMag >>= stripPrefix "(off) ")
    where mMag = stripPrefix "Magnifier " l
          def  = maybe ("",l) ("\xf84a",) mMag

-- Predefined glpyhs for layout descriptions
helperLayoutToIcon :: [Char] -> String
helperLayoutToIcon l
    | l == "ResizableTall"        = xmobarSymbol "\xe002"
    | l == "Mirror ResizableTall" = xmobarSymbol "\xe003"
    | l == "Full"        = xmobarSymbol "\xe000"
    | l == "ThreeCol"    = xmobarSymbol "\xe1de"
    | l == "TwoPanePersistent" = "2"
    | l == "Circle"      = "\xf192"
    | otherwise          = l
