import XMonad
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Tree (Tree(Node), Forest)
import System.Exit
import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders
import XMonad.Actions.TreeSelect
import XMonad.Actions.CopyWindow
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Circle
import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle
import XMonad.Layout.Renamed
import XMonad.Layout.SimpleFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Roledex
import XMonad.Layout.Accordion
import XMonad.Layout.NoBorders
import XMonad.Layout.TwoPanePersistent
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.WindowProperties (getProp32)
import XMonad.Prompt.AppendFile
-- Minimize imports
import XMonad.Layout.Minimize
import XMonad.Hooks.Minimize
import XMonad.Actions.Minimize
import XMonad.Layout.BoringWindows as BW
import XMonad.Layout.Hidden
import XMonad.Layout.LayoutModifier 
-- Personal Libraries
import Local.XmobarExtras
import Local.EwmhExtras
-- Scratchpad
import XMonad.Util.NamedScratchpad
-- Switch WM (testing)
import XMonad.Util.Replace


import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal      = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth   = 2

myModMask       = mod4Mask

myWorkspaces    = ["\xf015","\xf0ac","\xf1b6","\xf044","\xf121","\xf086","\xf6cc","\xe066","\xf1e5"]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#a15501"
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

    -- Minimize window
    , ((modm, xK_v), withFocused minimizeWindow)
    , ((modm .|. shiftMask, xK_v), withLastMinimized maximizeWindow)
    -- Mark boring
    , ((modm .|. controlMask, xK_v), markBoring)
    , ((modm .|. controlMask .|. shiftMask, xK_v), clearBoring)

    -- Toggle iconify window names for xmobar
    , ((modm, xK_s), toggleIconify)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Switch Window Managers without a new session
    , ((modm .|. shiftMask, xK_c), restart "/home/archie/scripts/wm-cycle" True )

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
   [((modm, xK_Pause),                spawn "picom --backend glx --vsync --xrender-sync-fence &")
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
    [((modm .|. shiftMask, xK_t),    spawn "/home/archie/scripts/toggle-trayer.sh")]

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

------------------------------------------------------------------------
-- Layouts:
--layoutOverrides = onWorkspace "steam" simpleFloat

myLayout = renamed [CutWordsLeft 1] $ minimize $ BW.boringWindows $ 
             onWorkspace "\xf1b6" (noBorders Full ||| tiled) $
             renamed [CutWordsLeft 1] $ smartSpacingWithEdge 5 $ 
             mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $
             avoidStrutsOn [D,L,R]  $ allLayouts
  where
     tiled   = ResizableTall nmaster delta ratio []
    --  tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100
     allLayouts = tiled ||| Mirror tiled ||| Full ||| TwoPanePersistent Nothing delta ratio ||| Circle 

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
    , namedScratchpadManageHook scratchpads
    ]
--    , manageDocks ]

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

myEventHook = minimizeEventHook'

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
  { ts_font = "xft:Noto Sans Mono:pixelsize=16"
  , ts_node = (0x00ffffff, 0xff111111)
  , ts_nodealt = (0x00ffffff, 0xff111111)
--  , ts_highlight = (0x00f2e750, 0xcc111111)
  , ts_highlight = (0x00f2e750, 0xff111111)
  , ts_background = 0x00000000

  }

tsActions :: Forest (TSNode (X ()))
tsActions = 
  [ Node (TSNode "Systemctl" "" (return ())) 
    [ Node (TSNode "Log Off" "" (io (exitWith ExitSuccess))) []
    , Node (TSNode "Suspend" "" (spawn "systemctl suspend")) []
    , Node (TSNode "Shutdown" "" (spawn "shutdown now")) []
    , Node (TSNode "Restart" "" (spawn "reboot")) []
    ]
  , Node (TSNode "Novideo" "" (spawn "sudo nvidia-settings")) []
  , Node (TSNode "XRandR" "" (spawn "xrandr --auto")) []
  , Node (TSNode "PulseAudio" "" (spawn "pavucontrol")) []
  , Node (TSNode "CPU Monitor" "" (runInTerm "" "sudo s-tui")) []
  , Node (TSNode "NetworkManager" "" (spawn "networkmanager_dmenu")) []
  ]

------------------------------------------------------------------------
-- Startup hook

myStartupHook = do
    spawnOnce "systemctl --user start xmonad-session.target" -- services only needed for a visual session
    spawnOnce "nitrogen --restore &" -- wallpapers
    spawnOnce "nvidia-settings --load-config-only" 
    spawnOnce "picom --backend glx --vsync --xrender-sync-fence &" 
    spawnOnce "/home/archie/scripts/toggle-trayer.sh"
    spawnOnce "dunst &"
--    spawn "sleep 5 && xrandr --auto" -- janky fix to novideo not detecting DP monitors on wakeup

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
