module Hooks.Keys   (
                      KeyState(..)
                    , ifKey
                    , keyDownEventHook
                    , keyUpEventHook
                    ) where

import              XMonad   hiding (keys)

import qualified    XMonad.Util.ExtensibleState as XS

import              Data.Monoid
import qualified    Data.Map as M   (Map, lookup)


data KeyState = Up | Down deriving ( Eq, Read )
instance ExtensionClass KeyState where initialValue = Up


ifKey :: KeyState -> X () -> X ()
ifKey s x = ( XS.get :: X KeyState )
                >>= \key ->
                    if key == s then return ()
                    else             XS.put (s)
                                  >> x
                                  >> (spawn $ "xdotool key End") -- simulated keypress fixes a flickering bug


keyUpEventHook :: M.Map ( KeyMask, KeySym ) ( X () ) -> Event -> X All
keyUpEventHook keys ev =
    handleKeyUpEvent ev keys
 >> return ( All True )
    where
        
        handleKeyUpEvent :: Event -> M.Map ( KeyMask, KeySym ) ( X () ) -> X ()
        handleKeyUpEvent ( KeyEvent { ev_event_type = t, ev_state = m, ev_keycode = code } ) ks
            | t == keyRelease =
            withDisplay $ \dpy -> do
                k  <- io $ keycodeToKeysym dpy code 0
                mClean <- cleanMask m
                userCodeDef () $ whenJust ( M.lookup ( mClean, k ) ks ) id
        handleKeyUpEvent _ _ = return ()


keyDownEventHook :: M.Map ( KeyMask, KeySym ) ( X () ) -> Event -> X All
keyDownEventHook keys ev =
    handleKeyDownEvent ev keys
 >> return ( All True )
    where

        handleKeyDownEvent :: Event -> M.Map ( KeyMask, KeySym ) ( X () ) -> X ()
        handleKeyDownEvent ( KeyEvent { ev_event_type = t, ev_state = m, ev_keycode = code } ) ks
            | t == keyPress =
            withDisplay $ \dpy -> do
                s  <- io $ keycodeToKeysym dpy code 0
                mClean <- cleanMask m
                userCodeDef () $ whenJust ( M.lookup ( mClean, s ) ks ) id
        handleKeyDownEvent _ _ = return ()
