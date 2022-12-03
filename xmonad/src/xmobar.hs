{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PostfixOperators #-}

import          Config.Prelude

-- Xmobar: Base
import          Xmobar

-- XMonad: Hooks
import          XMonad.Hooks.StatusBar.PP   (wrap, xmobarColor)

-- Custom: Plugin
import          Tray.Alsa 

-- Custom: Colors
import          Color.Colors



myFonts :: [ String ]
myFonts = 
    [
      myFont ++ "pixelsize=38"
    ]


myCommands :: [ Runnable ]
myCommands =
    [
      Run UnsafeXMonadLog
    , Run $ Cpu
        [ "--template" , "<total>%"
        , "--Low"      , "1"
        , "--High"     , "50"
        , "--high"     , colorRed ++ "," ++ colorGreen
        ] ( 1 `seconds` )
    , Run $ Memory
        [ "--template" , "<usedratio>%"
        , "--Low"      , "1"
        , "--High"     , "50"
        , "--high"     , colorRed ++ "," ++ colorGray 
        ] ( 1 `seconds` )
    , Run $ Network "enp8s0"
        [ "--template" , "↓<rx>kB↑<tx>kB"
        , "--Low"      , "1000"
        , "--High"     , "10000"
        , "--low"      , colorWhite ++ "," ++ colorBlue
        ] ( 1 `seconds` )
    , Run $ Date "%a %b %_d %Y %H:%M:%S" "date" ( 1 `seconds` )
    , Run $ AlsaTray colorGreen colorDarkGray 1
    , Run $ Com ( myPath ++ "script/xmobar-tray-cmus.sh" ) [] "cmus" 1
    ]
    where
        seconds :: Int -> Int
        seconds = (* 10)




main :: IO ()
main = 
    xmobar
  $ defaultConfig
    {
      font = concatMap ( wrap "" "," ) myFonts
    , additionalFonts = []
                
    , bgColor = colorBlack
    , fgColor = colorWhite
    , alpha = 255

    , position = OnScreen 1 Top

    , lowerOnStart = True
    , hideOnStart = False

    , commands = myCommands
    
    , sepChar = "%"
    , alignSep = "}{"
    , template = concat
        [
                                                " %UnsafeXMonadLog%}{"
        , xmobarColor colorBlack colorBlack     " "
        , xmobarColor colorBlack colorGreen     "  CPU: %cpu%  "
        , xmobarColor colorBlue colorGray       "  MEM: %memory%  "
        , xmobarColor colorWhite colorBlue      "  NET: %enp8s0%  "
        , xmobarColor colorBlack colorDarkCyan  "  %date%  "
        , xmobarColor colorGreen colorBlack     " %alsa% "
        , xmobarColor colorCyan colorBlack      "%cmus%"
        ]
    }
