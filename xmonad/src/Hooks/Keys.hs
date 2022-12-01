module Hooks.Keys   (
                      KeyState(..)
                    , ifKey
                    , keyDownEventHook
                    , keyUpEventHook
                    ) where

-- Base
import              XMonad

-- Util
import qualified    XMonad.Util.ExtensibleState as XS

-- Data
import              Data.Monoid
import qualified    Data.Map as M                       (Map, lookup)




data KeyState = Up | Down deriving ( Eq , Read )
instance ExtensionClass KeyState where initialValue = Up


ifKey :: KeyState -> X () -> X ()
ifKey s x =
        ( XS.get :: X KeyState )
    >>= \key ->
            if key == s then return ()
            else             XS.put (s)
                         >>  x
                         >>  (spawn $ "xdotool key End") -- simulated keypress fixes a flickering bug


keyDownEventHook :: M.Map ( KeyMask , KeySym ) ( X () ) -> Event -> X All
keyDownEventHook ks ev =
    handle ev keyPress ks
 >> return ( All True )


keyUpEventHook :: M.Map ( KeyMask , KeySym ) ( X () ) -> Event -> X All
keyUpEventHook ks ev =
    handle ev keyRelease ks
 >> return ( All True )


handle :: Event -> EventType -> M.Map ( KeyMask, KeySym ) ( X () ) -> X ()
handle ( KeyEvent { ev_event_type = t , ev_state = m , ev_keycode = code } ) t' ks
    | t == t' =
        withDisplay $ \dpy -> do
            k  <- io $ keycodeToKeysym dpy code 0
            m' <- cleanMask m
            userCodeDef () $ whenJust ( M.lookup ( m' , k ) ks ) id
handle _ _ _ = return ()
