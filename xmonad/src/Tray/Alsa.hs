module Tray.Alsa    (
                      AlsaTray(..)
                    ) where

-- Base
import  Xmobar

-- XMonad
import  XMonad.Hooks.StatusBar.PP   (xmobarColor)

-- System
import  System.Process              (readProcess)

-- Text
import  Text.Printf                 (printf)
import  Text.Regex.TDFA             ((=~))

-- Custom
import  Color.Colors                (colorBlack)




data AlsaTray = AlsaTray String String Int deriving ( Read , Show )
instance Exec AlsaTray where
        alias ( AlsaTray _  _  _ ) = "alsa"
        rate  ( AlsaTray _  _  r ) = r
        run   ( AlsaTray c1 c2 _ ) =
                readProcess "amixer" [ "get" , "Master" ] []
            >>= \raw ->
                    let ( vol , muted ) =   (
                                              tail $ init $ raw =~ "\\[[0-9]+%\\]"  :: String
                                            ,               raw =~ "\\[off\\]"      :: Bool
                                            )
                    in
                        let c = case muted of
                                    True  -> c2
                                    False -> c1
                        in
                            return $ xmobarColor c colorBlack ( printf "VOL: %4s" vol )
