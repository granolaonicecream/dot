{-# OPTIONS_GHC -Wno-deprecations #-}
-- Silencing deprecation warning ^
-- Could reimplement fullscreenStartup but whatever

--------------------------------------------------------------
-- Minor modification to XMonad.Hooks.EwmhDesktops that adds 
--  support for ignoring the default ewmhFullscreen event 
--  hook in favor of using the default xmonad fullscreen

module Local.EwmhExtras (
    ewmhFullscreen',
    toggleNoEwmhFullscreen
) where

import Data.Maybe (fromMaybe)
import Text.Printf (printf)

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.WindowProperties (getProp32)


-- | Add EWMH fullscreen functionality to the given config.
ewmhFullscreen' :: XConfig a -> XConfig a
ewmhFullscreen' c = c { startupHook     = startupHook c <+> fullscreenStartup
                     , handleEventHook = handleEventHook c <+> fullscreenEventHook' }

-- A modified version of EwmhDesktops fullscreen
-- Checks window for an xprop to determine whether or not to override the EWMH
--  fullscreen behavior.  If overriding, simply go to the next event hook in the chain
fullscreenEventHook' :: Event -> X All
fullscreenEventHook' (ClientMessageEvent _ _ _ dpy win typ (action:dats)) = do
  managed <- isClient win
  noEwmh <- getAtom "_NO_EWMH"
  wmstate <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  wstate <- fromMaybe [] <$> getProp32 wmstate win
  noEwmhState <- fromMaybe [] <$> getProp32 noEwmh win

  let isFull = fromIntegral fullsc `elem` wstate
      overrideFull = 1 `elem` noEwmhState

      -- Constants for the _NET_WM_STATE protocol:
      remove = 0
      add = 1
      toggle = 2
      chWstate f = io $ changeProperty32 dpy win wmstate aTOM propModeReplace (f wstate)

  when (managed && typ == wmstate && fi fullsc `elem` dats) $ do
    when (action == add || (action == toggle && not isFull)) $ do
      chWstate (fi fullsc:)
      unless overrideFull $ do
          windows $ W.float win $ W.RationalRect 0 0 1 1
    when (action == remove || (action == toggle && isFull)) $ do
      chWstate $ delete (fi fullsc)
      unless overrideFull $ do
          windows $ W.sink win

  return $ All True

fullscreenEventHook' _ = return $ All True

--------------------------------------------------------------
-- Mark a window to use old fullscreen behavior
toggleNoEwmhFullscreen :: Window -> X()
toggleNoEwmhFullscreen win = do
  noEwmh <- getAtom "_NO_EWMH"
  d <- asks display
  noEwmhState <- fromMaybe [] <$> getProp32 noEwmh win
  cName <- runQuery className win
  let overrideFull = 1 `elem` noEwmhState

  if overrideFull
    then do 
        io $ deleteProperty d win noEwmh
	spawn $ printf "dunstify -i preferences-desktop-display-symbolic.symbolic 'Enabled EWMH fullscreen for %s (%s)' -h string:x-dunst-stack-tag:fs%s" cName (show win) (show win)
    else do
	io $ changeProperty32 d win noEwmh cARDINAL propModeReplace [fromIntegral 1]
	spawn $ printf "dunstify -i preferences-desktop-display-symbolic.symbolic 'Disabled EWMH fullscreen for %s (%s)' -h string:x-dunst-stack-tag:fs%s" cName (show win) (show win)
