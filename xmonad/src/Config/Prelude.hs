module Config.Prelude   (
                          myPath
                        , myTerminal
                        , myBrowser
                        , myEditor
                        , myEditor'
                        , myFont                        
                        ) where


myPath :: String
myPath = "/home/wintcape/.config/xmonad/"

myFont :: String
myFont = "xft:PxPlus IBM VGA 8x16:"

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

myEditor, myEditor' :: String
myEditor  = "nvim" 
myEditor' = myTerminal ++ " -e " ++ myEditor ++ " "
