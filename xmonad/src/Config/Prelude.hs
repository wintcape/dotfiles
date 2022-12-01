module Config.Prelude   (
                          XApp(..)
                        , XAppType(..)
                        , xappCommand
                        , xappClassName
                        , myPath
                        , myFont
                        , myTerminal
                        , myBrowser
                        , myEditor
                        , myEditor'
                        ) where




myPath :: String
myPath = "/home/wintcape/.config/xmonad/"

myFont :: String
myFont = "xft:PxPlus IBM VGA 8x16:"




data XApp     = XApp String XAppType
data XAppType = CLI | GUI String

xappCommand , xappClassName :: XApp -> String
xappCommand   ( XApp s _ )         = s
xappClassName ( XApp _ ( GUI s ) ) = s
xappClassName ( XApp _ CLI )       = xappClassName myTerminal


myTerminal           :: XApp
myBrowser            :: XApp
myEditor , myEditor' :: XApp
myTerminal = XApp "alacritty" $ GUI "Alacritty"
myBrowser  = XApp "firefox"   $ GUI "firefox"
myEditor   = XApp "nvim"        CLI
myEditor'  = XApp ( ( xappCommand myTerminal ) ++ " -e " ++ ( xappCommand myEditor ) ) CLI
