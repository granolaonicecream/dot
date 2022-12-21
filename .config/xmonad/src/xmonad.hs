{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
import XMonad
import Data.Monoid
import Data.Tree (Tree(Node), Forest)
import System.Exit
import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders
import XMonad.Actions.TreeSelect
import XMonad.Actions.CopyWindow
import XMonad.Hooks.StatusBar
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.TwoPanePersistent
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.SpawnOnce
import XMonad.Prompt.AppendFile
-- Minimize imports
import XMonad.Layout.Minimize
import XMonad.Hooks.Minimize
import XMonad.Actions.Minimize
import XMonad.Layout.BoringWindows as BW
-- Personal Libraries
import XmobarExtras
import EwmhExtras
import QTileColumns
import Colors
-- Scratchpad
import XMonad.Util.NamedScratchpad
-- Switch WM (testing)
import XMonad.Util.Replace

import XMonad.Hooks.ServerMode

import XMonad.Layout.Gaps

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.Util.ExtensibleState as XS  -- Custom State

-- Testing
import XMonad.Layout.Groups.Examples
import XMonad.Layout.Groups.Helpers as GH
import XMonad.Layout.WindowNavigation

import XMonad.Hooks.ManageHelpers

import IconExtras


myTerminal      = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth   = 3

myModMask       = mod4Mask

myWorkspaces    = ["\xf015","\xf0ac","\xf1b6","\xf044","\xf121","\xf086","\xf6cc","\xe066","\xf1e5"]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#ae6419"
-- myNormalBorderColor  = "#a15501"
myFocusedBorderColor = "#f2e750"


------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- shuffle wall(p)aper
    , ((modm,               xK_p     ), spawn "python /home/archie/scripts/nitrogen-randomizer.py --once ~/Pictures/jensen")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

    -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm,               xK_z ), sendMessage ColLeft)
    , ((modm .|. shiftMask, xK_z ), sendMessage ColRight)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Reflect layout along X axis
    , ((modm .|. controlMask, xK_x), sendMessage $ Toggle REFLECTX)

    -- Reflect layout along Y axis
    , ((modm .|. controlMask, xK_y), sendMessage $ Toggle REFLECTY)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    , ((modm,               xK_F5    ), appendFilePrompt def "/home/archie/NOTES")

    -- Mini pager with desktop previews
    , ((modm .|. shiftMask, xK_Tab), spawn "/home/archie/xdpager/xdpager -c /home/archie/.config/xdpager/docked-rc -d Bottom" )

    -- Launch rofi to manage window selection, drun, and run
    , ((modm,               xK_Tab   ), spawn "rofi -combi-modi window,drun,ssh -modi combi,window,drun,run -show combi -show-icons -font 'monospace,Font Awesome 6 Free'")

    -- Launch rofimoji, an emoji selector
    , ((modm,               xK_Insert  ), spawn "rofimoji")

    -- Launch networkmanager_dmenu, which uses rofi
    , ((modm,               xK_F1  ), spawn "networkmanager_dmenu")

    -- Move focus to the next window
    , ((modm,               xK_j     ), BW.focusDown)
    --, ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), BW.focusUp  )
    --, ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), BW.focusMaster  )
--    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Shrink a window
    , ((modm, xK_Down), sendMessage MirrorShrink)

    -- Expand a window
    , ((modm, xK_Up), sendMessage MirrorExpand)


    , ((modm .|. shiftMask, xK_Up), GH.focusUp)
    , ((modm .|. shiftMask, xK_Down), GH.focusDown)
    , ((modm .|. controlMask, xK_Up), GH.focusGroupUp)
    , ((modm .|. controlMask, xK_Down), GH.focusGroupDown)

    , ((modm .|. shiftMask .|. controlMask, xK_Left), GH.moveToGroupUp False)
    , ((modm .|. shiftMask .|. controlMask, xK_Down), GH.swapDown)
    , ((modm .|. shiftMask .|. controlMask, xK_Up), GH.swapUp)
    , ((modm .|. shiftMask .|. controlMask, xK_Right), GH.moveToGroupDown False)
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap (setup by withEasySB)
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Toggle active window border (e.g. for fullscreen)
    , ((modm .|. shiftMask , xK_b     ), withFocused toggleBorder >> refresh)

    -- Mark active window to only fullscreen to its dimensions (no float)
    , ((modm, xK_f), withFocused toggleNoEwmhFullscreen)

    -- Mark active window to only fullscreen to its dimensions (no float)
    , ((modm, xK_F10), withFocused $ (\w -> writeWindowIcon (16,16) w >> return ()) )

    -- Minimize window
    , ((modm, xK_v), withFocused minimizeWindow)
    , ((modm .|. shiftMask, xK_v), withLastMinimized maximizeWindow)
    -- Mark boring
    , ((modm .|. controlMask, xK_v), markBoring)
    , ((modm .|. controlMask .|. shiftMask, xK_v), clearBoring)

    -- Toggle iconify window names for xmobar
    , ((modm, xK_s), toggleIconify)

    -- Cycle through gaps
    -- , ((modm, xK_F10), cycleGaps)

    -- Toggle sink-inputs for Snapcast
    , ((modm, xK_F2), spawn "$HOME/scripts/snapcast-toggle.sh on")
    , ((modm .|. shiftMask, xK_F2), spawn "$HOME/scripts/snapcast-toggle.sh off")

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "makeColors && xmonad --recompile && xmonad --restart")

    -- Switch Window Managers without a new session
--    , ((modm .|. shiftMask, xK_c), restart "/home/archie/scripts/wm-cycle" True )

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))

    , ((modm, xK_slash ), spawn "xdotool search --classname xdpager windowfocus")

    -- TreeSelect action menu
    , ((modm, xK_x), treeselectAction myTreeConf tsActions)
    , ((modm, xK_g), goToSelected $ def)

    -- Copy commands
    , ((modm, xK_a), windows copyToAll)
    , ((modm .|. shiftMask, xK_a), killAllOtherCopies)
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    ++

    -- Window spacing
    -- mod-equals, increase spacing
    -- mod-plus, decrease spacing
    [((modm, xK_equal),    sequence_ [incWindowSpacing 2, incScreenSpacing 2])
    ,((modm .|. shiftMask, xK_equal),    sequence_ [decWindowSpacing 2, decScreenSpacing 2])]

    ++

    -- Volume control
    [((modm, xK_minus),      spawn "/home/archie/scripts/paChangeVolume.sh -t")
    ,((modm, xK_bracketleft),  spawn "/home/archie/scripts/paChangeVolume.sh -v -5%")
    ,((modm, xK_bracketright), spawn "/home/archie/scripts/paChangeVolume.sh -v +5%")]

    ++

    -- Compositor control
   [((modm, xK_Pause),                spawn "picom &")
   ,((modm .|. shiftMask, xK_Pause),  spawn "killall picom")]

    ++

    -- Screen capture
    -- Currently depends on shotgun and rofi
    [((noModMask, xK_Print),         spawn "/home/archie/scripts/shotgun-wrapper.sh fullscreen 0")
    ,((modm, xK_Print), spawn "/home/archie/scripts/shotgun-rofi.sh")
    ]

    ++

    -- Scratchpads
    [( (modm .|. shiftMask, xK_d), namedScratchpadAction scratchpads "term") ]

    ++

    -- Trayer toggle script until styling nailed down
    [((modm .|. shiftMask, xK_t),    spawn "/home/archie/scripts/toggle-stalone.sh")]

    ++

    [((modm .|. controlMask              , xK_plus ), sendMessage Mag.MagnifyMore)
    ,((modm .|. controlMask              , xK_minus), sendMessage Mag.MagnifyLess)
    ,((modm .|. controlMask              , xK_o    ), sendMessage Mag.ToggleOff  )
    ,((modm .|. controlMask .|. shiftMask, xK_o    ), sendMessage Mag.ToggleOn   )
    ,((modm .|. controlMask              , xK_m    ), sendMessage Mag.Toggle     )
    ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w ))
--                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

-- newtype GapState = GapIndex Int deriving Show
-- instance ExtensionClass GapState where
--   initialValue = GapIndex 0

-- myGaps :: [GapSpec]
-- myGaps = [ [(R,0),(L,0),(U,20),(D,20)]
--          , [(U,0),(R,0),(L,20),(D,20)]
--          , [(D,0),(R,0),(L,20),(U,20)] ]

-- cycleGaps :: X()
-- cycleGaps = do
--   (GapIndex idx) <- XS.gets $ \(GapIndex i) -> 
--     let n = if i >= length myGaps - 1 then 0 else i+1 in GapIndex n
--   sendMessage (setGaps $ myGaps !! idx)
--   XS.put $ GapIndex idx

------------------------------------------------------------------------
-- Layouts:
--layoutOverrides = onWorkspace "steam" simpleFloat
myLayout = renamed [CutWordsLeft 1] $ minimize $ BW.boringWindows $
             --onWorkspace "\xf1b6" (noBorders Full ||| tiled) $
             onWorkspace "\xf1b6" (noBorders Full) $
             renamed [CutWordsLeft 1] $ smartSpacingWithEdge 5 $
             mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $
             Mag.magnifierOff $ windowNavigation allLayouts
            --  avoidStrutsOn [D,L,R]  $ Mag.magnifierOff $ windowNavigation allLayouts
  where
     tiled   = ResizableTall nmaster delta ratio []
    --  tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100
     -- qt = qtileCol
     -- mult = multiCol [0] 2 0.01 0.25
     allLayouts = tiled ||| Mirror tiled ||| Full ||| TwoPanePersistent Nothing delta ratio ||| rowOfColumns

------------------------------------------------------------------------
--Scratchpad setup
--
scratchpads =
    [ NS "term" "alacritty --title scratchpad" (title =? "scratchpad") defaultFloating

    ]


-- Window rules:
myManageHook = composeAll
    [ resource  =? "Toolkit"        --> insertPosition Below Older
    , className =? "Steam"          --> doShift (myWorkspaces!!2)
    , title     =? "Friends List"   --> doShift (myWorkspaces!!5) <+> doFloat
    --, className =? "xdpager"        --> placeHook (fixed (0,0)) <+> doFloat
    , className =? "xdpager"        --> doFloat
    , className =? "discord"        --> doShift (myWorkspaces!!5)
    , title     =? "xmessage"       --> doFloat
    , className     =? "mpv"       --> doSendToScreen (S 0) <+> doFloat
    , className =? "Tickr"         --> doFloat
    , className =? "Gsimplecal"    --> gsimplecalHook <+> doIgnore
    , className =? "Pavucontrol"    --> doFloat
    , isDialog --> doFloat
    , namedScratchpadManageHook scratchpads
    ]
--    , manageDocks ]

-- A ManageHook to allow a window to always appear on the same ScreenId (monitor)
doSendToScreen :: ScreenId -> ManageHook
doSendToScreen sid = ask >>= (\w -> doF . shifter w =<< maybeWs)
  where maybeWs = liftX $ screenWorkspace sid
        shifter win (Just ws) = W.shiftWin ws win
        shifter _   Nothing   = id

-- gsimplecal doesn't support configuration per screen
-- This hook naively assumes a horizontal 2560x1440 dual monitor setup
-- positioning gsimplecal based on the cursor position when invoked.
-- Necessary since focus following only works over managed windows 
-- (e.g. not over xmobar)
gsimplecalHook:: ManageHook
gsimplecalHook = ask >>= \w -> liftX $ withDisplay $ \dpy -> do
  (x,_) <- getPointer w dpy
  let xRight = if x < 2560 then (2560 :: Int) else 2560 * 2
      xPos = fromIntegral xRight - 250
  -- Move with XLib since we want to doIgnore in the same hook
  io (moveWindow dpy w xPos 25)
  mempty


-- Copied from X.H.Place
getPointer :: Window -> Display -> X (XMonad.Position,XMonad.Position)
getPointer w dpy = do
  (_,_,_,x,y,_,_,_) <- io $ queryPointer dpy w
  return (fromIntegral x,fromIntegral y)
------------------------------------------------------------------------
-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- Modified minimize event hook to prevent certain applications that act weird
-- (e.g. dota 2 tries to minimize itself if it loses focus, sets focused, etc)
-- Either apply the minimize hook (always returns All True) or noop and return
-- to caller to do the rest of the hooks
minimizeEventHook' :: Event -> X All
minimizeEventHook' msg@ClientMessageEvent{ev_window = w} = do
    c_name <- runQuery className w
    if (c_name == "dota2")
      then return (All True)     -- noop and continue
      else minimizeEventHook msg -- apply the minimize hook and continue
minimizeEventHook' _ = return (All True)

myEventHook = serverModeEventHook' (return [("foo", io $ xmessage "test")]) <+> minimizeEventHook' <+> iconCleanupHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()


-- Status Bar configurations
xmobarMain      = statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 0 /home/archie/.config/xmobar/xmobarrc" (clickablePP (myXmobarPP 0))
xmobarSecondary = statusBarPropTo "_XMONAD_LOG_2" "xmobar -x 1 /home/archie/.config/xmobar/xmobarrc-2" (clickablePP (myXmobarPP 1))

xmobarProvider :: ScreenId -> IO StatusBarConfig
xmobarProvider 0 = pure xmobarMain
xmobarProvider 1 = pure xmobarSecondary
xmobarProvider _ = mempty

--------------------------------------------------------------
-- TreeSelect stuff
myTreeConf :: TSConfig (X ())
myTreeConf = def
  { ts_font = "xft:TerminessTTF Nerd Font:pixelsize=16"
  , ts_node = (0x00d0902f, 0xff351409)
  , ts_nodealt = (0x00d0902f, 0xff351409)
--  , ts_highlight = (0x00f2e750, 0xcc111111)
  , ts_highlight = (0xffdddaa0, 0xff64523f)
  , ts_background = 0x00000000
  , ts_originY = 25
  }

tsActions :: Forest (TSNode (X ()))
tsActions =
  [ Node (TSNode "Systemctl" "" (return ()))
    [ Node (TSNode "Log Off" "" (io (exitWith ExitSuccess))) []
    , Node (TSNode "Suspend" "" (spawn "systemctl suspend")) []
    , Node (TSNode "Shutdown" "" (spawn "shutdown now")) []
    , Node (TSNode "Restart" "" (spawn "reboot")) []
    ]
  , Node (TSNode "Audio/Video" "" (return ()))
    [  Node (TSNode "Novideo" "" (spawn "sudo nvidia-settings")) []
    , Node (TSNode "XRandR" "" (spawn "xrandr --auto")) []
    , Node (TSNode "PulseAudio" "" (spawn "pavucontrol")) []
    ]
  , Node (TSNode "NetworkManager" "" (spawn "networkmanager_dmenu")) []
  ]

------------------------------------------------------------------------
-- Startup hook

myStartupHook = do
    spawnOnce "systemctl --user start xmonad-session.target" -- services only needed for a visual session
    spawnOnce "nvidia-settings --load-config-only"
    spawnOnce "nitrogen --restore &" -- wallpapers
    spawnOnce "picom &"
    spawnOnce "/home/archie/scripts/toggle-stalone.sh"
    spawnOnce "snapserver &" -- TODO: move to a user systemd service
    spawnOnce "dunst &"
    spawn "sleep 5 && xrandr --auto" -- janky fix to novideo not detecting DP monitors on wakeup

------------------------------------------------------------------------
-- Define main to run xmonad.  
main = do
    replace
    xmonad $ ewmhFullscreen' $ ewmh
      $ withUrgencyHook NoUrgencyHook
      $ dynamicEasySBs xmobarProvider
      $ docks defaults

defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

-- | Outdated.  TBD replace
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
