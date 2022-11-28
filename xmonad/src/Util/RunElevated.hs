module Util.RunElevated (
                          runInTerm'
                        , runInTerm''
                        ) where

import Config.Prelude           (myPath, myTerminal)

import XMonad

import XMonad.Actions.SpawnOn   (spawnOn)
import XMonad.Util.SpawnOnce    (spawnOnOnce)


runInTerm :: String -> String -> String -> String
runInTerm msg opts cmd = myTerminal ++ " " ++ opts
                      ++ " -e " ++ myPath ++ "script/elevate"
                      ++ " \"" ++ msg ++ "\" \"" ++ cmd ++ "\""

runInTerm', runInTerm'' :: String -> String -> String -> X ()
runInTerm'  msg opts cmd = spawnOn      "sys" $ runInTerm msg opts cmd
runInTerm'' msg opts cmd = spawnOnOnce  "sys" $ runInTerm msg opts cmd
