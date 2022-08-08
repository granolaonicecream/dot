module Local.XmobarExtras (
    toggleIconify,
    myXmobarPP
) where

import XMonad
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.List (stripPrefix)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar.PP (xmobarFont)
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.WindowProperties (getProp32)
import XMonad.Prelude (find,liftM)
import qualified XMonad.Util.ExtensibleState as XS  -- Custom State
import qualified XMonad.StackSet as W

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
toggleIconify = XS.modify $ apply not

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

-- Like Xmonad.Util.Loggers but also styles the text based on:
--  * window class name
--  * ExtensibleState TitleState
--  * window minimization status
logTitlesOnScreen'
  :: ScreenId           -- ^ Screen to log the titles on
  -> (String -> String) -- ^ Formatting for the focused   window
  -> (String -> String) -- ^ Formatting for the unfocused window
  -> (String -> String) -- ^ Formatting for the unfocused window
  -> Logger
logTitlesOnScreen' sid formatFoc formatUnfoc formatMin = (`withScreen` sid) $ \screen -> do
  let focWin = fmap W.focus . W.stack . W.workspace $ screen
      wins   = maybe [] W.integrate . W.stack . W.workspace $ screen
  winNames <- traverse (fmap show . getName) wins
  winClasses <- traverse (runQuery className) wins
  winHidden <- traverse (isWindowHidden) wins  
  sw <- XS.get :: X TitleState
  let nameFmt = decorateWindowName $ unwrap sw
      decorated = zipWith nameFmt winNames winClasses
      formatWindow :: Window -> String -> Bool -> String
      formatWindow w n h = if h 
        then (xmobarRestore w $ formatMin n) 
        else xmobarMinimize w $ (if Just w == focWin then formatFoc n else formatUnfoc n)
  pure . Just
       . unwords
       $ zipWith3 formatWindow wins decorated winHidden

-- | Like 'logTitlesOnScreen', but directly use the "focused" screen
-- (the one with the currently focused workspace).
logTitles' :: (String -> String) -> (String -> String) -> Logger
logTitles' formatFoc formatUnfoc = do
  sid <- gets $ W.screen . W.current . windowset
  logTitlesOnScreen' sid formatFoc formatUnfoc formatUnfoc

-- Decorate the window name with an icon if it exists
-- If window titles should be iconified, use icon or first char
decorateWindowName :: Bool -> String -> String -> String
decorateWindowName iconify name className = fromMaybe noIcon $ liftM lifter icon
  where icon = classNameToIcon className
        firstChar [] = ""
        firstChar s  = [s!!0]
        noIcon = if iconify then firstChar name else name
        lifter = if iconify then (flip (++) " ") else (\i -> wrap i name " ")

-- Known window glyph icons to prepend (Nerd Font codepoints)
classNameToIcon :: String -> Maybe String
classNameToIcon cn = case cn of
  "firefox" -> Just "\xf269"
  "Alacritty" -> Just "\xf120"
  "VSCodium" -> Just "\xf1c9"
  "discord" -> Just "\xfb6e"
  "Steam" -> Just "\xf1b6"
  "xdpager" -> Just "\xfa6f"
  otherwise -> Nothing

-----------------------------------------------------------------------
-- Color constants
color1 = "#eac440" -- main highlight color (focus, active ws)
--color1 = "#fdd870" -- main highlight color (focus, active ws)
--color1 = "#f2e750"
--color2 = "#363636"
color2 = "#351409" -- box background
--color3 = "#6b6b6b"
color3 = "#d0902f" -- darkened text
color4 = "#a15501" -- box border, visible workspace

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
    , ppLayout = normalBox .  xmobarColor' color3 color2 . wrap " " " " . layoutToIcon
    , ppOrder = \[ws, l, _, wins] -> [workspaceBox ws, l, wins]
    , ppExtras = [logTitlesOnScreen' sid formatFocused formatUnfocused formatMinimized]
--    , ppSep = " : "
    , ppSep = " "
    , ppRename = (\n _ -> xmobarFont 2 n)  -- pixelsize in Nerd Fonts aren't great

    }
  where
    formatFocused = focusedColor . highlightBox . wrap "\xe0bc " " " . ppWindow
    formatUnfocused = unfocusedColor . normalBox . wrap "  " " " . ppWindow
    formatMinimized = minimizedBox . xmobarColor' "grey" "#404040" . wrap "  " " " . ppWindow

    workspaceBox =  xmobarAction "~/scripts/pager-wrapper.sh" "2" . normalBox . xmobarColor' color4 color2 . wrap " " " "

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "---" else w) . shorten 32 . filter (not . isEmoji)

    normalBox = xmobarBorder "Full" color4 1 
    highlightBox = xmobarBorder "Full" color1 1
    minimizedBox = xmobarBorder "Full" "grey" 1
    focusedColor = xmobarColor' color1 color2
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
-- Assumes the only description modifier prefixes are 
-- ReflectX/ReflectY and changes them into shortform X/Y
-- TODO: make actual icons and flip them while accounting for
-- layouts without symbols.
layoutToIcon :: String -> String
layoutToIcon l = case stripPrefix "Reflect" l of
    Just (d:' ':suffix) -> d:(layoutToIcon suffix)
    Nothing -> helperLayoutToIcon l

helperLayoutToIcon l
    | l == "ResizableTall"        = xmobarSymbol "\xe002" 
    | l == "Mirror ResizableTall" = xmobarSymbol "\xe003" 
    | l == "Full"        = xmobarSymbol "\xe000" 
    | l == "ThreeCol"    = xmobarSymbol "\xe1de" 
    | l == "TwoPanePersistent" = "2"
    | l == "Circle"      = "\xf192"
    | otherwise          = l
