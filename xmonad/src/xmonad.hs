import              Config.Prelude

-- XMonad: Base
import              XMonad                   hiding ((|||))
import              XMonad.Prelude
import qualified    XMonad.StackSet as W

-- XMonad: Actions
import              XMonad.Actions.CopyWindow       (kill1)
import              XMonad.Actions.CycleWS          (shiftNextScreen)
import              XMonad.Actions.OnScreen         (onlyOnScreen, viewOnScreen)
import              XMonad.Actions.Search           (dictionary, duckduckgo, google, hoogle,
                                                     thesaurus, wikipedia, youtube)
import              XMonad.Actions.SpawnOn          (manageSpawn, spawnOn)
import              XMonad.Actions.UpdateFocus      (focusOnMouseMove)
import              XMonad.Actions.WithAll          (sinkAll, killAll)

-- XMonad: Hooks
import              XMonad.Hooks.EwmhDesktops
import              XMonad.Hooks.ManageDocks        (avoidStruts, docks, manageDocks)
import              XMonad.Hooks.ManageHelpers      (doHideIgnore, doRectFloat, doSink)
import              XMonad.Hooks.StatusBar
import              XMonad.Hooks.StatusBar.PP

-- Custom: Hooks
import              Hooks.Keys                      (KeyState(..), ifKey, keyDownEventHook, keyUpEventHook)

-- XMonad: Layout modifiers
import              XMonad.Layout            hiding ((|||))
import              XMonad.Layout.Fullscreen
import              XMonad.Layout.Grid
import              XMonad.Layout.LayoutCombinators ((|||), JumpToLayout)
import              XMonad.Layout.NoBorders
import              XMonad.Layout.PerWorkspace
import              XMonad.Layout.Renamed           (Rename(Replace), renamed)
import              XMonad.Layout.ShowWName

-- XMonad: Prompt
import              XMonad.Prompt

-- Custom: Prompt
import              Prompt.Man                      (manPrompt)
import              Prompt.Search                   (searchPrompt)
import              Prompt.Terminal                 (terminalPrompt)

-- XMonad: Util
import              XMonad.Util.ClickableWorkspaces (clickablePP)
import              XMonad.Util.EZConfig            (mkKeymap)
import              XMonad.Util.NamedScratchpad
import              XMonad.Util.SpawnOnce           (spawnOnOnce)
import qualified    XMonad.Util.ExtensibleState as XS

-- Custom: Util
import              Util.RunElevated                (runInTermElevated, runInTermElevatedOnce)

-- System
import              System.Exit

-- Data
import              Data.Monoid
import              Data.Ratio
import qualified    Data.Map as M                   (Map, fromList)

-- Custom: Common
import              Color.Colors                    (colorWhite, colorGray, colorRed, colorDarkGray)




-- Cursor movement controls focus?
--
myFocusFollowsMouse :: Bool 
myFocusFollowsMouse = True


-- Pass click to new application on change of focus via click?
--
myClickJustFocuses :: Bool                  
myClickJustFocuses = False


-- Window border
--
myBorderWidth                   :: Dimension
myBorderFColor, myBorderUFColor :: String

myBorderWidth   = 2
myBorderFColor  = colorWhite                -- focused window border color
myBorderUFColor = colorDarkGray             -- unfocused window border color


-- Workspaces
--
myWorkspaces :: [ WorkspaceId ]
myWorkspaces = [ "vim" , "web" , "ful" , "mus" , "rec" , "doc" , "sys" ]
--                 0       1       2       3       4       5       6

mySWNConfig :: SWNConfig                    -- show workspace name
mySWNConfig = def
    {
      swn_font    = myFont ++ "pixelsize=148"
    , swn_fade    = 1.0
    , swn_bgcolor = colorDarkGray
    , swn_color   = colorWhite
    }


-- Layouts
--
myFullLayout        = noBorders . avoidStruts $                  renamed [ Replace "full" ] Full
myGridLayout        =             avoidStruts $ myFullLayout ||| renamed [ Replace "grid" ] Grid
myFullscreenLayout  =          fullscreenFull $ myFullLayout ||| (noBorders $ Full)


-- Status bar (xmobar)
--
myStatusBar :: StatusBarConfig
myStatusBar = statusBarProp
    ( "$HOME/.local/bin/xmobar" )
    ( clickablePP $ filterOutWsPP [ "NSP" ] myXmobarPP )
    where
        myXmobarPP :: PP
        myXmobarPP = def
            {
              ppCurrent         = xmobarColor ( colorDarkGray ++ "," ++ colorWhite    ) ""              . wrap "  " "  "
            , ppVisible         = xmobarColor ( colorWhite    ++ "," ++ colorGray     ) ""              . wrap "  " "  "
            , ppHidden          = xmobarColor ( colorWhite    ++ "," ++ colorDarkGray ) ""              . wrap " *" "  "
            , ppHiddenNoWindows = xmobarColor ( colorWhite    ++ "," ++ colorDarkGray ) ""              . wrap "  " "  "
            , ppUrgent          = xmobarColor ( colorWhite    ++ "," ++ colorRed      ) ""              . wrap " !" "! "
            , ppTitle           = xmobarColor ( colorDarkGray ++ "," ++ colorWhite    ) "" . shorten 64 . wrap "  " "  "
            , ppSep             = "  "
            , ppOrder           = \( ws : _ : t ) -> [ ws ] ++ t
            }


-- Scratchpads
--
myScratchpads :: [ NamedScratchpad ]
myScratchpads =
    [
      NS "sh" spawnTerminal findTerminal manageTerminal
    , NS "htop" spawnHtop findHtop manageHtop
    , NS "qalc" spawnCalculator findCalculator manageCalculator
    , NS "pulsemixer" spawnPulsemixer findPulsemixer managePulsemixer
    ]
    where
        -- Terminal on demand
        spawnTerminal   = myTerminal ++ " --title 'Terminal on Demand!'"
        findTerminal    = title =? "Terminal on Demand!"
        manageTerminal  = customFloating $ W.RationalRect l t w h
            where                        -- centered floating layout
                w = 0.9
                h = 0.9
                l = 0.05
                t = 0.05 
        -- Resource monitor
        spawnHtop   = myTerminal ++ " --title 'htop' -e htop"
        findHtop    = title =? "htop"
        manageHtop  = customFloating $ W.RationalRect l t w h
            where
                w = 1.0
                h = 0.5
                l = 0
                t = 0.5 
        -- Calculator
        spawnCalculator     = "qalculate-gtk"
        findCalculator      = className =? "Qalculate-gtk"
        manageCalculator    = customFloating $ W.RationalRect l t w h
            where                            -- centered floating layout
                w = 0.5
                h = 0.5
                l = 0.25
                t = 0.25
        -- Audio controller 
        spawnPulsemixer     = myTerminal ++ " --title 'pulsemixer' -e pulsemixer"
        findPulsemixer      = title =? "pulsemixer"
        managePulsemixer    = customFloating $ W.RationalRect l t w h
            where                            -- right floating layout
                w = 1.0
                h = 0.2
                l = 0
                t = 0.018825


-- Prompts
--
myPromptConfig :: XPConfig
myPromptConfig = def
    {
      font                  = myFont ++ "pixelsize=40"
    , bgColor               = colorDarkGray
    , fgColor               = colorWhite
    , fgHLight              = colorDarkGray
    , bgHLight              = colorWhite
    , borderColor           = colorDarkGray
    , promptBorderWidth     = 0
    , position              = Bottom
    , height                = 84
    , maxComplRows          = Just 1
    , promptKeymap          = defaultXPKeymap
    , defaultText           = ""
    , autoComplete          = Nothing
    , showCompletionOnTab   = False
    , historySize           = 256
    }




-- Key bindings
--
myModMask :: KeyMask
myModMask = mod4Mask


myKeyBindings :: XConfig l -> M.Map ( KeyMask , KeySym ) ( X () )
myKeyBindings conf@( XConfig { XMonad.modMask = myModMask } ) = mkKeymap conf $
    [

    -- Session 
      ( "M-<Escape>"   , spawnOn "sys" $ myTerminal ++ " -e "
                                      ++ myPath ++ "script/recompile" )         -- recompile and restart, or display error
    , ( "M-S-<Escape>" , io exitSuccess )                                       -- kill X
    , ( "M-<F5>"       , runInTermElevated "Shutdown?" "" "shutdown -h now" )   -- elevated: shutdown
    , ( "M-<F6>"       , runInTermElevated "Reboot?"   "" "reboot" )            -- elevated: reboot

    -- Workspace navigation
    , ( "<KP_Insert>"   , windows $ W.greedyView ( myWorkspaces !! 0 ) )      -- move focus to workspace n
    , ( "<KP_End>"      , windows $ W.greedyView ( myWorkspaces !! 1 ) )
    , ( "<KP_Down>"     , windows $ W.greedyView ( myWorkspaces !! 2 ) )
    , ( "<KP_Next>"     , windows $ W.greedyView ( myWorkspaces !! 3 ) )
    , ( "<KP_Left>"     , windows $ W.greedyView ( myWorkspaces !! 4 ) )
    , ( "<KP_Begin>"    , windows $ W.greedyView ( myWorkspaces !! 5 ) )
    , ( "<KP_Right>"    , windows $ W.greedyView ( myWorkspaces !! 6 ) )
    , ( "M-<KP_Insert>" , windows $ W.shift      ( myWorkspaces !! 0 ) )      -- shift focused window to workspace n
    , ( "M-<KP_End>"    , windows $ W.shift      ( myWorkspaces !! 1 ) )
    , ( "M-<KP_Down>"   , windows $ W.shift      ( myWorkspaces !! 2 ) )
    , ( "M-<KP_Next>"   , windows $ W.shift      ( myWorkspaces !! 3 ) )
    , ( "M-<KP_Left>"   , windows $ W.shift      ( myWorkspaces !! 4 ) )
    , ( "M-<KP_Begin>"  , windows $ W.shift      ( myWorkspaces !! 5 ) )
    , ( "M-<KP_Right>"  , windows $ W.shift      ( myWorkspaces !! 6 ) )

    -- Current workspace
    , ( "M-S-<Space>" , refresh )                                               -- reset to default layout
    , ( "M-<Tab>"     , windows W.focusDown )                                   -- move focus to next window
    , ( "M--"         , sendMessage Shrink )                                    -- shrink master window
    , ( "M-="         , sendMessage Expand )                                    -- expand master window
    , ( "M-q"         , kill1 )                                                 -- kill current window
    , ( "M-S-q"       , killAll )                                               -- kill all windows in current workspace
    , ( "M-t"         , withFocused $ windows . W.sink )                        -- Push floating window back to tile
    , ( "M-S-t"       , sinkAll )                                               -- Push ALL floating windows to tile
    
    -- Application spawning
    , ( "M-M1-<Return>" , spawn           myTerminal)                         -- alacritty
    , ( "M-M1-e"        , spawnOn "vim" $ myEditor')                          -- nvim
    , ( "M-M1-b"        , spawn           myBrowser)                          -- firefox
    , ( "M-<F1>"        , runInTermElevated "Launch steam?"                   -- elevated: steam
                                            ( " --config-file " ++ myPath ++ "script/alacritty-chroot.yml" )
                                            "steam" )
    , ( "M-<F2>"        , runInTermElevated "Chroot steam?"                   -- elevated: steam-chroot
                                            ( " --config-file " ++ myPath ++ "script/alacritty-chroot.yml" )
                                            "steam-chroot" )
    , ( "M-<F3>"        , runInTermElevated "Unchroot steam?"                 -- elevated: steam-unchroot
                                            ( " --config-file " ++ myPath ++ "script/alacritty-chroot.yml" )
                                            "steam-unchroot" )

    -- Prompt / Scratchpad
    , ( "M-S-<Return>" , terminalPrompt myPromptConfig )                        -- launch run prompt
    , ( "M-C-m"        , manPrompt      myPromptConfig 0 "doc")                 -- launch man prompt
    , ( "M-C-s s"      , searchPrompt   myPromptConfig myBrowser google )       -- query google
    , ( "M-C-s a"      , searchPrompt   myPromptConfig myBrowser duckduckgo )   -- query duckduckgo
    , ( "M-C-s d"      , searchPrompt   myPromptConfig myBrowser dictionary )   -- query dictionary
    , ( "M-C-s t"      , searchPrompt   myPromptConfig myBrowser thesaurus )    -- query thesaurus
    , ( "M-C-s w"      , searchPrompt   myPromptConfig myBrowser wikipedia )    -- query wikipedia
    , ( "M-C-s y"      , searchPrompt   myPromptConfig myBrowser youtube )      -- query youtube
    , ( "M-C-s h"      , searchPrompt   myPromptConfig myBrowser hoogle )       -- query hoogle
    , ( "M-<Return>"   , namedScratchpadAction myScratchpads "sh" )             -- toggle terminal
    , ( "M-z"          , namedScratchpadAction myScratchpads "htop" )           -- toggle htop
    , ( "M-c"          , namedScratchpadAction myScratchpads "qalc" )           -- toggle qalc
    , ( "M-m"          , namedScratchpadAction myScratchpads "pulsemixer" )     -- toggle pulsemixer

    -- Audio controls
    , ( "M-<Page_Up>"   , spawn "amixer -D pulse sset Master 5%+" )              -- master device volume+
    , ( "M-<Page_Down>" , spawn "amixer -D pulse sset Master 5%-" )              -- master device volume-
    , ( "M-<Home>"      , spawn "cmus-remote --volume +1%" )                     -- cmus volume+
    , ( "M-<End>"       , spawn "cmus-remote --volume -1%" )                     -- cmus volume-
    , ( "M-<F9>"        , spawn "cmus-remote --pause" )                          -- cmus pause toggle
    , ( "M-<F10>"       , spawn "cmus-remote --prev" )                           -- cmus prev track
    , ( "M-<F11>"       , spawn "cmus-remote --next" )                           -- cmus next track
    , ( "M-<Pause>"     , spawn "amixer set Master toggle" )                     -- master alsa mute toggle
   
    -- Dummy entries required for XMonad key detection
    -- (see myKeyUpBindings, myKeyDownBindings)
    , ( "M-<Space>" , return () )
    , ( "M-`"       , return () )

    ]


myKeyUpBindings :: XConfig l -> M.Map ( KeyMask , KeySym ) ( X () )
myKeyUpBindings conf@( XConfig { XMonad.modMask = myModMask } ) = mkKeymap conf $
    [
      ( "M-`"       , ifKey Up ( sendMessage $ JumpToLayout "full" ) )              -- switch to full layout
    , ( "M-<Space>" , ifKey Up $ XS.put Up )                                        -- do nothing but pass the key
    ]


myKeyDownBindings :: XConfig l -> M.Map ( KeyMask , KeySym ) ( X () )
myKeyDownBindings conf@( XConfig { XMonad.modMask = myModMask } ) = mkKeymap conf $
    [
      ( "M-`"       , ifKey Down ( sendMessage $ JumpToLayout "grid" ) )            -- switch to grid layout
    , ( "M-<Space>" , ifKey Down $ sendMessage   NextLayout )                       -- switch to next layout
    ]




myMouseBindings :: XConfig l -> M.Map ( ButtonMask , Button ) ( Window -> X () )
myMouseBindings _ = M.fromList $
    [

      ( ( myModMask , 1 ) , ( \w ->
                                    focus w
                                >>  mouseMoveWindow w
                                >>  windows W.shiftMaster ) )                       -- M+LCLICK: position window
    , ( ( myModMask , 3 ) , ( \w ->
                                    focus w
                                >>  mouseResizeWindow w
                                >>  windows W.shiftMaster ) )                       -- M+RCLICK: resize window
    , ( ( myModMask , 2 ) , ( \w ->
                                    focus w
                                >>  windows W.shiftMaster ) )                       -- M+MOUSE3: bring to front
            
    -- Soundfx
    , ( ( 0, 9 ) , const $ spawn "cmus-remote --queue --file \"$(ls archive-i/soundpad/* | shuf -n 1)\"" )
    , ( ( 0, 8 ) , const $ spawn "cmus-remote --queue --stop" )
            
    ]




-- Hooks
-- 


myLayoutHook =
    showWName' mySWNConfig $
    onWorkspace ( myWorkspaces !! 0 ) myGridLayout $
    onWorkspace ( myWorkspaces !! 1 ) myGridLayout $
    onWorkspace ( myWorkspaces !! 2 ) myFullscreenLayout $
    onWorkspace ( myWorkspaces !! 3 ) myGridLayout $
    onWorkspace ( myWorkspaces !! 4 ) myFullscreenLayout $
    onWorkspace ( myWorkspaces !! 5 ) myGridLayout $
    onWorkspace ( myWorkspaces !! 6 ) myGridLayout $
    Full


myManageHook :: XMonad.Query ( Data.Monoid.Endo WindowSet )
myManageHook = composeAll
        [

        -- Floaters! (lol)
            className =? "confirm"                                                                                          -->     myDoFloat
        ,   className =? "file_progress"                                                                                    -->     myDoFloat
        ,   className =? "dialog"                                                                                           -->     myDoFloat
        ,   className =? "download"                                                                                         -->     myDoFloat
        ,   className =? "error"                                                                                            -->     myDoFloat
        ,   className =? "notification"                                                                                     -->     myDoFloat
        ,   className =? "splash"                                                                                           -->     myDoFloat
        ,   className =? "toolbar"                                                                                          -->     myDoFloat

        -- Thunar
        ,   className =? "Thunar"                                                                                           -->     doRectFloat ( W.RationalRect (1 % 48) (1 % 48) (1 % 4) (1 % 4) )
        , ( className =? "Thunar"    <&&>   title =? "Create New Folder" )                                                  -->     doRectFloat ( W.RationalRect (1 % 48) (1 % 48) (1 % 4) (1 % 4) )
        , ( className =? "Alacritty" <&&> ( title =? "thunar" <||> "thunar " ?^ title ) )                                   -->     doShift "sys"

        -- sys workspace
        ,   className =? "Xmessage"                                                                                         -->     doHideIgnore
                                                                                                                            --  >>  doShift "sys"

        -- Image viewing / capture
        ,   className =? "vlc"                                                                                              -->     liftX   (windows $ viewOnScreen 1 "ful")
                                                                                                                                >>  doShift "ful"
        ,   className =? "obs"                                                                                              -->     liftX   (windows $ viewOnScreen 0 "rec")
                                                                                                                                >>  doShift "rec"
        , ( className =? "Alacritty" <&&> ( title =? "vlc" <||> "vlc " ?^ title ) )                                         -->     doShift "sys"
        , ( className =? "Alacritty" <&&> ( title =? "fim" <||> "fim " ?^ title ) )                                         -->     doShift "sys"
        , ( className =? "Alacritty" <&&> ( title =? "obs" <||> "obs " ?^ title ) )                                         -->     doShift "sys"
        
        -- Minecraft
        ,  "Minecraft" ?^ className                                                                                         -->     doShift "ful"
        , ( className =? "minecraft-launcher" <||> className =? "Minecraft Launcher" )                                      -->     doShift "ful"
        , ( className =? "Minecraft Launcher" <&&>   title =? "Minecraft game output" )                                     -->     doShift "sys"
        , ( className =? "Alacritty"          <&&> ( title =? "minecraft-launcher" <||> "minecraft-launcher " ?^ title ) )  -->     doShift "sys"

        -- Audacity
        ,   className =? "Audacity"                                                                                         -->     liftX   (windows $ viewOnScreen 0 "rec")
                                                                                                                                >>  doShift "rec"
        , ( className =? "Audacity"  <&&>   title =? "Select one or more files" )                                           -->     myDoFloat
        , ( className =? "Alacritty" <&&> ( title =? "audacity" <||> "audacity " ?^ title ) )                               -->     doShift "sys"
        
        -- Steam
        ,   className =? "Steam"                                                                                            -->     liftX   (windows $ viewOnScreen 1 "ful")
                                                                                                                                >>  liftX   (windows $ viewOnScreen 0 "sys")
                                                                                                                                >>  doShift "ful"
        , ( className =? "Steam"     <&&> title =? "Friends List" )                                                         -->     doHideIgnore
        , ( className =? "Steam"     <&&> title =? "Steam - News" )                                                         -->     doHideIgnore
        ,   className =? "csgo_linux64"                                                                                     -->     doShift "ful"
                                                                                                                                >>  doSink
        
        -- Firefox
        , ( className =? "firefox"   <&&>   title =? "Mozilla Firefox" )                                                    -->     doShift' "web"
        , ( className =? "firefox"   <&&>   title =? "File Upload" )                                                        -->     myDoFloat
        , ( className =? "firefox"   <&&>   title =? "Open File" )                                                          -->     myDoFloat
        , ( className =? "firefox"   <&&>   title =? "Library" )                                                            -->     myDoFloat
        , ( className =? "firefox"   <&&>   title =? "Choose Application" )                                                 -->     myDoFloat
        , ( className =? "Alacritty" <&&> ( title =? "firefox" <||> "firefox " ?^ title ) )                                 -->     doShift "sys"
        
        -- Custom terminals
        , ( className =? "Alacritty" <&&> ( title =? "cmus"      <||> "cmus "      ?^ title ) )                             -->     doShift' "mus"
        , ( className =? "Alacritty" <&&> ( title =? "alacritty" <||> "alacritty " ?^ title ) )                             -->     doShift "sys"

        ] <+> namedScratchpadManageHook myScratchpads <+> manageDocks <+> manageSpawn
        
        where
        
            -- (?^)
            -- Returns True if x isPrefixOf q
            (?^) :: ( Eq a , Functor m ) => [ a ] -> m [ a ] -> m Bool
            x ?^ q = fmap ( x `isPrefixOf` ) q

            -- myDoFloat
            -- Custom manager for floating windows. Has a fixed size of 50% viewport dims
            myDoFloat :: ManageHook
            myDoFloat = doRectFloat ( W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2) )
            
            -- doShift'
            -- Variation of doShift that switches to the workspace
            doShift' = doF . liftM2 (.) W.greedyView W.shift


myStartupHook :: X ()
myStartupHook = 
        ( windows $ onlyOnScreen 0 "sys" )
    >>  ( windows $ onlyOnScreen 1 "vim" )
    >>  ( runInTermElevatedOnce "Launch sudo nvim?" "" myEditor )
    >>  ( spawnOnOnce                         "vim" $  myEditor' )


myEventHook :: Event -> X All
myEventHook ev = 
        focusOnMouseMove
    <+> keyDownEventHook ( myKeyDownBindings myConfig )
    <+> keyUpEventHook   ( myKeyUpBindings   myConfig )
      $ ev




---------- MAIN -----------
---------------------------

myConfig = def
    {
      terminal              = myTerminal
    , focusFollowsMouse     = myFocusFollowsMouse
    , clickJustFocuses      = myClickJustFocuses
    , borderWidth           = myBorderWidth
    , modMask               = myModMask
    , workspaces            = myWorkspaces
    , normalBorderColor     = myBorderUFColor
    , focusedBorderColor    = myBorderFColor
               
    , keys                  = myKeyBindings
    , mouseBindings         = myMouseBindings

    , layoutHook            = myLayoutHook
    , manageHook            = myManageHook
    , handleEventHook       = myEventHook
    , startupHook           = myStartupHook
    , logHook               = mempty
    }

main :: IO ()
main = xmonad . withSB myStatusBar . ewmh . docks $ myConfig
