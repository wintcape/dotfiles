module Config.Prelude   (
                          XApp(..)
                        , XAppType(..)
                        , xappCommand
                        , xappCommand'
                        , xappClassName
                        , myPath
                        , myLogFile
                        , myFont
                        , myTerminal
                        , myBrowser
                        , myEditor
                        , myAudioController
                        ) where




myPath :: String
myPath = "/home/wintcape/.config/xmonad/"

myLogFile :: String
myLogFile = myPath ++ ".log"

myFont :: String
myFont = "xft:PxPlus IBM VGA 8x16:"


data XApp     = XApp String XAppType
data XAppType = CLI | GUI String
xappCommand , xappCommand' , xappClassName :: XApp -> String
xappCommand   ( XApp s _ )         = s
xappCommand'  ( XApp s ( GUI _ ) ) = s
xappCommand'  ( XApp s CLI )       = ( xappCommand myTerminal ) ++ " -e " ++ s
xappClassName ( XApp _ ( GUI s ) ) = s
xappClassName ( XApp _ CLI )       = xappClassName myTerminal


myTerminal        :: XApp
myBrowser         :: XApp
myEditor          :: XApp
myAudioController :: XApp
myTerminal        = XApp "alacritty" $ GUI "Alacritty"
myBrowser         = XApp "firefox"   $ GUI "firefox"
myEditor          = XApp "nvim"        CLI
myAudioController = XApp "pulsemixer"  CLI
