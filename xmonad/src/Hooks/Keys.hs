module Hooks.Keys   (
                      KeyState(..)
                    , ifKey
                    , keyDownEventHook
                    , keyUpEventHook
                    ) where


import              XMonad

import qualified    XMonad.Util.ExtensibleState as XS

import              Data.Monoid
import qualified    Data.Map as M  (Map, lookup)




data KeyState = Up | Down deriving (Eq, Read)
instance ExtensionClass KeyState where initialValue = Up


ifKey :: KeyState -> X () -> X ()
ifKey s x = (XS.get :: X KeyState)
                >>= \key ->
                        if key == s then return ()
                        else             XS.put (s)
                                      >> x
                                      >> (spawn $ "xdotool key End") -- simulated keypress fixes a flickering bug


keyUpEventHook :: M.Map ( KeyMask, KeySym ) ( X () ) -> Event -> X All
keyUpEventHook ks ev =
    handleKeyUpEvent ev ks
 >> return (All True)


handleKeyUpEvent :: Event -> M.Map ( KeyMask, KeySym ) ( X () ) -> X ()
handleKeyUpEvent (KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code}) ks
    | t == keyRelease =
    withDisplay $ \dpy -> do
        s  <- io $ keycodeToKeysym dpy code 0
        mClean <- cleanMask m
        userCodeDef () $ whenJust (M.lookup (mClean, s) ks) id
handleKeyUpEvent _ _ = return ()


keyDownEventHook :: M.Map ( KeyMask, KeySym ) ( X () ) -> Event -> X All
keyDownEventHook ks ev =
    handleKeyDownEvent ev ks
 >> return (All True)


handleKeyDownEvent :: Event -> M.Map ( KeyMask, KeySym ) ( X () ) -> X ()
handleKeyDownEvent (KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code}) ks
    | t == keyPress =
    withDisplay $ \dpy -> do
        s  <- io $ keycodeToKeysym dpy code 0
        mClean <- cleanMask m
        userCodeDef () $ whenJust (M.lookup (mClean, s) ks) id
handleKeyDownEvent _ _ = return ()
