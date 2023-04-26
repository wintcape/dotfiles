module Prompt.Man   (
                      manPrompt
                    ) where

-- XMonad: Base
import XMonad
import XMonad.Prelude

-- XMonad: Prompt
import XMonad.Prompt
import XMonad.Prompt.Shell              (split)

-- XMonad: Actions
import XMonad.Actions.OnScreen          (viewOnScreen)
import XMonad.Actions.SpawnOn           (spawnOn)

-- System
import System.Directory
import System.FilePath                  (dropExtensions, (</>))
import System.IO
import System.Process

-- Control
import qualified Control.Exception as E




data Man = Man
instance XPrompt Man where showXPrompt Man = "man "


manPrompt :: XPConfig -> WorkspaceId -> X ()
manPrompt conf wid =
        io getMans
    >>= \mans ->
            mkXPrompt Man conf
            ( manCompl conf mans )
            ( \input ->
                windows ( viewOnScreen 0 wid )
            >>  asks ( terminal . config )
            >>= \t ->
                    spawnOn wid $ t ++ " -e man " ++ input
            )
    where
        
        getMans :: IO [ String ]
        getMans = do
            paths <- do
                let getout cmd = getCommandOutput cmd `E.catch` \E.SomeException{} -> return ""
                p1 <- getout "manpath -g 2>/dev/null"
                p2 <- getout "manpath 2>/dev/null"
                return $ intercalate ":" $ lines $ p1 ++ p2
            let sects    = [ "man" ++ show n | n <- [ 1..9 :: Int ] ]
                dirs     = [ d </> s | d <- split ':' paths, s <- sects ]
            mans <- forM ( nub dirs ) $ \d -> do
                exists <- doesDirectoryExist d
                if exists then map dropExtensions <$> getDirectoryContents d
                else           return []
            return $ uniqSort $ concat mans

        manCompl :: XPConfig -> [ String ] -> String -> IO [ String ]
        manCompl c mans s | s == "" || last s == ' ' = return []
                          | otherwise                =
                                lines <$> getCommandOutput ( "bash -c 'compgen -A file " ++ s ++ "'" )
                            >>= \f ->
                                    mkComplFunFromList c ( f ++ mans ) s

        getCommandOutput :: String -> IO String
        getCommandOutput s =
                runInteractiveCommand s
            >>= \( pin , pout , perr , _ ) ->
                    hClose pin
                >>  hGetContents pout
                >>= \out ->
                        E.evaluate ( length out )
                    >>  hClose perr
                    >>  return out
