module Hooks.Keys   (
                      KeyState(..)
                    , keyDownEventHook
                    , keyUpEventHook
                    ) where

-- XMonad: Base
import              XMonad

-- XMonad: Util
import qualified    XMonad.Util.ExtensibleState as XS

-- Data
import              Data.Monoid
import qualified    Data.Map as M                       (Map, lookup)




data KeyState = Up | Down deriving ( Eq , Read )
instance ExtensionClass KeyState where initialValue = Up


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
        withDisplay $ \dpy ->
                ( io $ keycodeToKeysym dpy code 0 )
            >>= \k ->
                    ( cleanMask m )
                >>= \m' ->            
                        let s = case () of
                                () | t == keyRelease -> Up
                                   | t == keyPress   -> Down
                        in
                                ( XS.get :: X KeyState )
                            >>= \s' ->
                                    case () of
                                    () | s' == s   ->   return ()
                                       | otherwise ->   ( XS.put ( s ) )
                                                    >>  ( userCodeDef () $ whenJust ( M.lookup ( m' , k ) ks ) id )
handle _ _ _ = return ()
