module Prompt.Terminal ( terminalPrompt
                       ) where

-- XMonad: Base
import XMonad

-- XMonad: Prompt
import XMonad.Prompt
import XMonad.Prompt.Shell  ( getCommands
                            , getShellCompl
                            )

-- Custom: Util
import Util.Run             ( runInTerm )


data Terminal = Terminal
instance XPrompt Terminal where
    showXPrompt Terminal  = ""
    completionToCommand _ = escape
        where
            escape :: String -> String
            escape []       = ""
            escape ( x : xs )
                | isSpecialChar x = '\\' : x : escape xs
                | otherwise       =        x : escape xs
                where
                    isSpecialChar :: Char -> Bool
                    isSpecialChar =  flip elem " &\\@\"'#?$*()[]{};"


terminalPrompt :: XPConfig -> X ()
terminalPrompt c =
        io getCommands
    >>= \cmds ->
            mkXPrompt Terminal c
            ( getShellCompl cmds $ searchPredicate c )
            (
                \input ->
                    runInTerm ( "--title \"" ++ input ++ "\"" ) input
            )
