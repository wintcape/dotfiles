module Util.RunElevated (
                          runInTermElevated
                        , runInTermElevatedOnce
                        ) where

import Config.Prelude           (myPath, myTerminal)

-- Base
import XMonad

-- Actions
import XMonad.Actions.OnScreen  (viewOnScreen)
import XMonad.Actions.SpawnOn   (spawnOn)

-- Util
import XMonad.Util.SpawnOnce    (spawnOnOnce)




command :: String -> String -> String -> String
command msg opts cmd = myTerminal ++ " " ++ opts
                      ++ " -e " ++ myPath ++ "script/elevate"
                      ++ " \"" ++ msg ++ "\" \"" ++ cmd ++ "\""


runInTermElevated, runInTermElevatedOnce :: String -> String -> String -> X ()
runInTermElevated     msg opts cmd =
        ( windows $ viewOnScreen 1 "sys" )
    >>  ( spawnOn    "sys" $ command msg opts cmd )
runInTermElevatedOnce msg opts cmd =
        spawnOnOnce "sys" $ command msg opts cmd
