module Util.Run (
                  runInTerm
                , runInTermOn
                , runInTermElevated
                , runInTermOnce
                , runInTermOnOnce
                , runInTermElevatedOnce
                ) where

import Config.Prelude

-- Base
import XMonad

-- Actions
import XMonad.Actions.OnScreen  (viewOnScreen)
import XMonad.Actions.SpawnOn   (spawnOn)

-- Util
import XMonad.Util.SpawnOnce    (spawnOnce, spawnOnOnce)




commandStr :: String -> String -> String
commandStr opt cmd = ( xappCommand myTerminal ) ++ " " ++ opt ++ " -e "
                  ++ myPath ++ "script/run \"" ++ cmd ++ "\""

elevatedCommandStr :: String -> String -> String -> String
elevatedCommandStr msg opt cmd = commandStr opt $ "echo " ++ msg ++ ";sudo " ++ cmd ++ ";"


runInTerm   , runInTermOnce   ::                String -> String -> X ()
runInTermOn , runInTermOnOnce :: WorkspaceId -> String -> String -> X ()
runInTerm           opt cmd = spawn           $ commandStr opt cmd
runInTermOn     wid opt cmd = spawnOn     wid $ commandStr opt cmd
runInTermOnce       opt cmd = spawnOnce       $ commandStr opt cmd
runInTermOnOnce wid opt cmd = spawnOnOnce wid $ commandStr opt cmd

runInTermElevated , runInTermElevatedOnce :: String -> String -> String -> X ()
runInTermElevated msg opt cmd =
        ( windows $ viewOnScreen 1 "sys" )
    >>  ( spawnOn    "sys" $ elevatedCommandStr msg opt cmd )
runInTermElevatedOnce msg opt cmd =
        spawnOnOnce "sys" $ elevatedCommandStr msg opt cmd
