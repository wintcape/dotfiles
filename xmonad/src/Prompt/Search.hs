module  Prompt.Search   (
                          searchPrompt
                        ) where

-- Base
import  XMonad

-- Prompt
import  XMonad.Prompt

-- Actions
import  XMonad.Actions.Search   (Browser, SearchEngine(..), search)

-- Data
import  Data.List               (isPrefixOf)




newtype Search = Search String
instance XPrompt Search where
    showXPrompt   ( Search name ) = "query [" ++ name ++ "] "
    nextCompletion _              = getNextCompletion
    commandToComplete _ c         = c


searchPrompt :: XPConfig -> Browser -> SearchEngine -> X ()
searchPrompt c b ( SearchEngine n s ) =
        historyCompletionP ( showXPrompt ( Search n ) `isPrefixOf` )
    >>= \hc ->
            mkXPrompt ( Search n ) c hc $ search b s
