import              Config.Defaults

-- XMonad: Base
import              XMonad                       hiding ( ( ||| )
                                                        , JumpToLayout ( .. )
                                                        )
import              XMonad.Prelude
import qualified    XMonad.StackSet as W

-- XMonad: Actions
import              XMonad.Actions.CopyWindow           ( kill1 )
import              XMonad.Actions.CycleWS              ( nextScreen
                                                        , shiftNextScreen
                                                        )
import              XMonad.Actions.OnScreen             ( onlyOnScreen
                                                        , viewOnScreen
                                                        )
import              XMonad.Actions.Search               ( dictionary
                                                        , duckduckgo
                                                        , google
                                                        , hoogle
                                                        , thesaurus
                                                        , wikipedia
                                                        , youtube
                                                        )
import              XMonad.Actions.SpawnOn              ( manageSpawn
                                                        , spawnOn
                                                        )
import              XMonad.Actions.UpdateFocus          ( focusOnMouseMove )
import              XMonad.Actions.WithAll              ( sinkAll
                                                        , killAll
                                                        )

-- XMonad: Hooks
import              XMonad.Hooks.EwmhDesktops
import              XMonad.Hooks.ManageDocks            ( avoidStruts
                                                        , docks
                                                        , manageDocks
                                                        )
import              XMonad.Hooks.ManageHelpers          ( ( /=? )
                                                        , currentWs
                                                        , doHideIgnore
                                                        , doRectFloat
                                                        , doSink
                                                        )
import              XMonad.Hooks.StatusBar
import              XMonad.Hooks.StatusBar.PP

-- Custom: Hooks
import              Hooks.Keys                          ( keyDownEventHook
                                                        , keyUpEventHook
                                                        )

-- XMonad: Layout modifiers
import              XMonad.Layout.Fullscreen
import              XMonad.Layout.GridVariants          ( Orientation ( .. )
                                                        , SplitGrid ( .. )
                                                        )
import              XMonad.Layout.LayoutCombinators     ( ( ||| )
                                                        , JumpToLayout ( .. )
                                                        )
import              XMonad.Layout.NoBorders
import              XMonad.Layout.PerWorkspace
import              XMonad.Layout.Renamed               ( Rename ( Replace )
                                                        , renamed
                                                        )
import              XMonad.Layout.ShowWName
import              XMonad.Layout.TwoPane

-- XMonad: Prompt
import              XMonad.Prompt

-- Custom: Prompt
import              Prompt.Man                          ( manPrompt )
import              Prompt.Search                       ( searchPrompt )
import              Prompt.Terminal                     ( terminalPrompt )

-- XMonad: Util
import              XMonad.Util.ClickableWorkspaces     ( clickablePP )
import              XMonad.Util.EZConfig                ( mkKeymap )
import              XMonad.Util.NamedScratchpad
import              XMonad.Util.SpawnOnce               ( spawnOnOnce )
import qualified    XMonad.Util.ExtensibleState as XS

-- Custom: Util
import              Util.Run                            ( runInTermOn
                                                        , runInTermElevated
                                                        , runInTermOnOnce
                                                        , runInTermElevatedOnce
                                                        )

-- System
import              System.Exit

-- Data
import              Data.Monoid
import              Data.Ratio
import qualified    Data.Map as M                       ( Map
                                                        , fromList
                                                        )

-- Custom: Common
import              Config.Colors




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
myBorderWidth                    :: Dimension
myBorderFColor , myBorderUFColor :: XColor

myBorderWidth   = 2
myBorderFColor  = colorDarkGray            -- focused window border color
myBorderUFColor = colorBlack                -- unfocused window border color


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
myFullLayout        = noBorders . avoidStruts $ 
                            renamed [ Replace "full" ]
                                Full

myGridLayout        = lessBorders Screen . avoidStruts $
                            renamed [ Replace "grid" ] (   ( SplitGrid L 1 1 ( 1 % 2 ) ( 1 % 1 ) ( 1 % 20 ) )
                                                       ||| ( SplitGrid T 1 1 ( 1 % 2 ) ( 1 % 1 ) ( 1 % 20 ) )
                                                       )
                        ||| myFullLayout

myFullscreenLayout    = fullscreenFull $
                            myFullLayout
                        ||| ( noBorders $ Full )


-- Status bar (xmobar)
--
myStatusBar :: StatusBarConfig
myStatusBar = statusBarProp
    (  myPath ++ "../../.cache/xmonad/xmobar" )
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
      NS "term" spawnTerminal        findTerminal        manageTerminal
    , NS "top"  spawnTop             findTop             manageTop
    , NS "calc" spawnCalculator      findCalculator      manageCalculator
    , NS "ac"   spawnAudioController findAudioController manageAudioController
    ]
    where
        -- Terminal on demand
        spawnTerminal  = ( xappCommand myTerminal ) ++ " --title 'Terminal on Demand!'"
        findTerminal   = title =? "Terminal on Demand!"
        manageTerminal = customFloating $ W.RationalRect l t w h
            where                        -- center floating layout
                w = 0.9
                h = 0.9
                l = 0.05
                t = 0.05 
        -- Resource monitor
        spawnTop  = ( xappCommand myTerminal ) ++ " --title 'htop' -e htop"
        findTop   = title =? "htop"
        manageTop = customFloating $ W.RationalRect l t w h
            where                    -- bottom floating layout
                w = 1.0
                h = 0.65
                l = 0
                t = 0.35
        -- Calculator
        spawnCalculator  = "qalculate-gtk"
        findCalculator   = className =? "Qalculate-gtk"
        manageCalculator = customFloating $ W.RationalRect l t w h
            where                           -- center floating layout
                w = 0.5
                h = 0.5
                l = 0.25
                t = 0.25
        -- Audio controller 
        spawnAudioController  = ( xappCommand myTerminal ) ++ " --title '" ++ ( xappCommand myAudioController ) ++ "' -e "
                             ++ ( xappCommand myAudioController )
        findAudioController   = title =? ( xappCommand myAudioController )
        manageAudioController = customFloating $ W.RationalRect l t w h
            where                                -- top floating layout
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
myKeyBindings conf = mkKeymap conf $
    [

    -- Session 
      ( "M-<Escape>"   ,    ( windows $ viewOnScreen 0 "sys" )                                  -- recompile and restart
                        >>  ( runInTermOn "sys" "--title 'Recompiling. . .'" ( myPath ++ "build && xmonad --restart" ) )
      )
    , ( "M-S-<Escape>" , io exitSuccess )                                                       -- kill X
    , ( "M-<F5>"       , runInTermElevated "Shutdown?" "--title 'Shutdown?'" "shutdown -h now" )-- elevated: shutdown
    , ( "M-<F6>"       , runInTermElevated "Reboot?"   "--title 'Reboot?'"   "reboot" )         -- elevated: reboot
    
    -- Workspace navigation
    , ( "<KP_Enter>"    , nextScreen )                                                          -- move focus to opposite display
    , ( "<KP_Insert>"   , windows $ W.greedyView ( myWorkspaces !! 0 ) )                        -- move focus to workspace n
    , ( "<KP_End>"      , windows $ W.greedyView ( myWorkspaces !! 1 ) )
    , ( "<KP_Down>"     , windows $ W.greedyView ( myWorkspaces !! 2 ) )
    , ( "<KP_Next>"     , windows $ W.greedyView ( myWorkspaces !! 3 ) )
    , ( "<KP_Left>"     , windows $ W.greedyView ( myWorkspaces !! 4 ) )
    , ( "<KP_Begin>"    , windows $ W.greedyView ( myWorkspaces !! 5 ) )
    , ( "<KP_Right>"    , windows $ W.greedyView ( myWorkspaces !! 6 ) )
    , ( "M-<KP_Enter>"  , shiftNextScreen )                                                     -- shift focused window to workspace on opposite display
    , ( "M-<KP_Insert>" , windows $ W.shift      ( myWorkspaces !! 0 ) )                        -- shift focused window to workspace n
    , ( "M-<KP_End>"    , windows $ W.shift      ( myWorkspaces !! 1 ) )
    , ( "M-<KP_Down>"   , windows $ W.shift      ( myWorkspaces !! 2 ) )
    , ( "M-<KP_Next>"   , windows $ W.shift      ( myWorkspaces !! 3 ) )
    , ( "M-<KP_Left>"   , windows $ W.shift      ( myWorkspaces !! 4 ) )
    , ( "M-<KP_Begin>"  , windows $ W.shift      ( myWorkspaces !! 5 ) )
    , ( "M-<KP_Right>"  , windows $ W.shift      ( myWorkspaces !! 6 ) )

    -- Current workspace
    , ( "M-S-<Space>" , refresh )                                                               -- refresh layout
    , ( "M-<Tab>"     , windows W.focusDown )                                                   -- move focus to next window
    , ( "M-a"         , windows W.swapMaster )                                                  -- swap current window with master window
    , ( "M--"         , sendMessage Shrink )                                                    -- shrink master window
    , ( "M-="         , sendMessage Expand )                                                    -- expand master window
    , ( "M-q"         , kill1 )                                                                 -- kill current window
    , ( "M-S-q"       , killAll )                                                               -- kill all windows in current workspace
    , ( "M-t"         , withFocused $ windows . W.sink )                                        -- push floating window back to tile
    , ( "M-S-t"       , sinkAll )                                                               -- push all floating windows to tile
    
    -- Application spawning
    , ( "M-M1-<Return>" , spawn         $ xappCommand myTerminal )                              -- alacritty
    , ( "M-M1-e"        , spawnOn "vim" $ xappCommand' myEditor )                               -- nvim
    , ( "M-M1-b"        , spawn         $ xappCommand myBrowser )                               -- firefox
    , ( "M-<F1>"        , runInTermElevated   "Launch steam?"                                   -- elevated: steam
                                            ( "--title 'steam'          --config-file " ++ myPath ++ "../alacritty/alacritty-chroot.toml" )
                                              "steam"
      )
    , ( "M-<F2>"        , runInTermElevated   "Chroot steam?"                                   -- elevated: steam-chroot
                                            ( "--title 'steam-chroot'   --config-file " ++ myPath ++ "../alacritty/alacritty-chroot.toml" )
                                              "steam-chroot"
      )
    , ( "M-<F7>"        , runInTermElevated                                                     -- elevated: text editor
                         ( "Launch sudo " ++ ( xappCommand myEditor ) ++ "?" ) "" $ xappCommand myEditor
      )

    -- Prompt / Scratchpad
    , ( "M-S-<Return>" , terminalPrompt myPromptConfig )                                        -- launch run prompt
    , ( "M-C-m"        , manPrompt      myPromptConfig "doc")                                   -- launch man prompt
    , ( "M-C-s"        , searchPrompt   myPromptConfig ( xappCommand myBrowser ) google )       -- query google
    , ( "M-C-a"        , searchPrompt   myPromptConfig ( xappCommand myBrowser ) duckduckgo )   -- query duckduckgo
    , ( "M-C-d"        , searchPrompt   myPromptConfig ( xappCommand myBrowser ) dictionary )   -- query dictionary
    , ( "M-C-t"        , searchPrompt   myPromptConfig ( xappCommand myBrowser ) thesaurus )    -- query thesaurus
    , ( "M-C-w"        , searchPrompt   myPromptConfig ( xappCommand myBrowser ) wikipedia )    -- query wikipedia
    , ( "M-C-y"        , searchPrompt   myPromptConfig ( xappCommand myBrowser ) youtube )      -- query youtube
    , ( "M-C-h"        , searchPrompt   myPromptConfig ( xappCommand myBrowser ) hoogle )       -- query hoogle
    , ( "M-<Return>"   , namedScratchpadAction myScratchpads "term" )                           -- toggle terminal
    , ( "M-z"          , namedScratchpadAction myScratchpads "top" )                            -- toggle htop
    , ( "M-c"          , namedScratchpadAction myScratchpads "calc" )                           -- toggle qalc
    , ( "M-m"          , namedScratchpadAction myScratchpads "ac" )                             -- toggle pulsemixer

    -- Audio controls
    , ( "M-<Page_Up>"   , spawn "amixer -D pulse sset Master 5%+" )                             -- master device volume+
    , ( "M-<Page_Down>" , spawn "amixer -D pulse sset Master 5%-" )                             -- master device volume-
    , ( "M-<Home>"      , spawn "cmus-remote --volume +1%" )                                    -- cmus volume+
    , ( "M-<End>"       , spawn "cmus-remote --volume -1%" )                                    -- cmus volume-
    , ( "M-<F9>"        , spawn "cmus-remote --pause" )                                         -- cmus pause toggle
    , ( "M-<F10>"       , spawn "cmus-remote --prev" )                                          -- cmus prev track
    , ( "M-<F11>"       , spawn "cmus-remote --next" )                                          -- cmus next track
    , ( "M-<Pause>"     , spawn "amixer set Master toggle" )                                    -- master alsa mute toggle
   
    -- Dummy entries required to pass keys to XMonad
    -- (see myKeyUpBindings, myKeyDownBindings)
    , ( "M-`"       , return () )
    , ( "M-<Space>" , return () )

    ]


myKeyUpBindings :: XConfig l -> M.Map ( KeyMask , KeySym ) ( X () )
myKeyUpBindings conf = mkKeymap conf $
    [
      ( "M-`"       ,   ( windows $ W.swapMaster )                                              -- switch to full layout
                     >> ( sendMessage $ JumpToLayout "full" )
      )
    , ( "M-<Space>" , return () )                                                               -- do nothing but pass the key
    ]


myKeyDownBindings :: XConfig l -> M.Map ( KeyMask , KeySym ) ( X () )
myKeyDownBindings conf = mkKeymap conf $
    [
      ( "M-`"       , sendMessage $ JumpToLayout "grid" )                                       -- switch to grid layout
    , ( "M-<Space>" , sendMessage   NextLayout )                                                -- toggle current layout
    ]


myMouseBindings :: XConfig l -> M.Map ( ButtonMask , Button ) ( Window -> X () )
myMouseBindings _ = M.fromList $
    [

      (( myModMask , 1 )    , ( \w ->                                                           -- M+LCLICK: position window
                                    focus w
                                >>  mouseMoveWindow w
                                >>  windows W.shiftMaster
      ))
    , (( myModMask , 3 )    , ( \w ->                                                           -- M+RCLICK: resize window
                                    focus w
                                >>  mouseResizeWindow w
                                >>  windows W.shiftMaster
      ))
    , (( myModMask , 2 )    , ( \w ->                                                           -- M+MOUSE3: bring to front
                                    focus w
                                >>  windows W.shiftMaster
      ))
            
    -- Soundfx
    --, ( ( 0, 9 ) , const $ spawn "cmus-remote -C 'set continue=false';cmus-remote --queue --file \"$(ls archive-i/soundpad/* | shuf -n 1)\";" )
    --, ( ( 0, 8 ) , const $ spawn "cmus-remote --queue --stop" )
            
    ]




-- Hooks
-- 


myLayoutHook =
    showWName' mySWNConfig $
    onWorkspace ( myWorkspaces !! 0 ) myGridLayout       $
    onWorkspace ( myWorkspaces !! 1 ) myGridLayout       $
    onWorkspace ( myWorkspaces !! 2 ) myFullscreenLayout $
    onWorkspace ( myWorkspaces !! 3 ) myGridLayout       $
    onWorkspace ( myWorkspaces !! 4 ) myFullscreenLayout $
    onWorkspace ( myWorkspaces !! 5 ) myGridLayout       $
    onWorkspace ( myWorkspaces !! 6 ) myGridLayout       $
    Full


myManageHook :: XMonad.Query ( Data.Monoid.Endo WindowSet )
myManageHook = composeAll
        [

        -- Floaters! (lol)
            className =? "confirm"                                                          --> myDoFloat
        ,   className =? "file_progress"                                                    --> myDoFloat
        ,   className =? "dialog"                                                           --> myDoFloat
        ,   className =? "download"                                                         --> myDoFloat
        ,   className =? "error"                                                            --> myDoFloat
        ,   className =? "notification"                                                     --> myDoFloat
        ,   className =? "splash"                                                           --> myDoFloat
        ,   className =? "toolbar"                                                          --> myDoFloat
        ,   className =? "vken"                                                             --> doFloat

        -- Thunar
        ,   className =? "Thunar"                                                           --> doRectFloat ( W.RationalRect (1 % 48) (1 % 48) (1 % 4) (1 % 4) )

        ,   (    className =? "Thunar"
            <&&> title     =? "Create New Folder"
            )                                                                               --> doRectFloat ( W.RationalRect (1 % 48) (1 % 48) (1 % 4) (1 % 4) )

        -- sys workspace
        ,   className =? "Xmessage"                                                         --> doHideIgnore -- temporary
                                                                                            --  >>  doShift "sys"
        ,   (    className =? ( xappClassName myTerminal )
            <&&> (  title =? "thunar"             <||> "thunar "             ?^ title
            <||>    title =? "vlc"                <||> "vlc "                ?^ title
            <||>    title =? "obs"                <||> "obs "                ?^ title
            <||>    title =? "minecraft-launcher" <||> "minecraft-launcher " ?^ title
            <||>    title =? "audacity"           <||> "audacity "           ?^ title    
            <||>    title =? "firefox"            <||> "firefox "            ?^ title
            <||>    title =? "fontforge"          <||> "fontforge "          ?^ title
            <||>    title =? ( xappCommand myPDFViewer )
            <||>    ( ( xappCommand myPDFViewer ) ++ " " ) ?^ title
            ))                                                                              --> doShift "sys"

        -- Image viewing / capture
        ,   className =? "vlc"                                                              --> liftX   ( windows $ viewOnScreen 1 "ful" )
                                                                                            >>  doShift "ful"

        ,   className =? "obs"                                                              --> liftX   ( windows $ viewOnScreen 0 "rec" )
                                                                                            >>  doShift "rec"

        -- Minecraft
        , "Minecraft" ?^ className                                                          --> liftX   ( windows $ viewOnScreen 1 "ful" )
                                                                                           >>  liftX   ( windows $ viewOnScreen 0 "sys" )
                                                                                            >>  doShift "ful"
        , (    className =? "minecraft-launcher"
          <||> className =? "Minecraft Launcher"
          )                                                                                 --> liftX   ( windows $ viewOnScreen 1 "ful" )
                                                                                            >>  liftX   ( windows $ viewOnScreen 0 "sys" )
                                                                                            >>  doShift "ful"
        ,   (    className =? "Minecraft Launcher"
            <&&> title     =? "Minecraft game output"
            )                                                                               --> doShift "sys"


        -- Audacity
        ,   className =? "Audacity"                                                         --> liftX   ( windows $ viewOnScreen 1 "rec" )
                                                                                            >>  doShift "rec"
        ,   (    className =? "Audacity"
            <&&> title     =? "Select one or more files"
            )                                                                               --> myDoFloat

        -- Steam
        ,   className =? "Steam"                                                            --> liftX   ( windows $ viewOnScreen 1 "ful" )
                                                                                            >>  liftX   ( windows $ viewOnScreen 0 "sys" )
                                                                                            >>  doShift "ful"
        ,   (      className =? "Steam"
            <&&> ( title     =? "Friends List"
            <||>   title     =? "Steam - News"
            ))                                                                              --> doHideIgnore

        ,   className =? "csgo_linux64"                                                     --> doShift "ful"
                                                                                            >>  doSink
        
        -- Firefox
        ,   (    className =? "firefox"
            <&&> title     =? "Mozilla Firefox"
            )                                                                               --> doShift' "web"

        ,   (      className =? "firefox"
            <&&> ( title     =? "File Upload"
            <||>   title     =? "Open File"
            <||>   title     =? "Library"
            <||>   title     =? "Choose Application"
            ))                                                                              --> myDoFloat
        
        -- Custom terminals
        ,   (      className =? ( xappClassName myTerminal )
            <&&> ( title     =? "cmus" <||> "cmus " ?^ title
            ))                                                                              --> doShift' "mus"
        
        -- PDF viewer
        ,   (    className =? ( xappClassName myPDFViewer )
            <&&> currentWs =? "vim"
            )                                                                               --> liftX ( sendMessage $ JumpToLayout "grid" )
                                                                                            >>  liftX ( windows W.swapMaster )
                                                                                            >>  doSink
        ,   (    className =? ( xappClassName myPDFViewer )
            <&&> currentWs /=? "vim"
            )                                                                               --> liftX ( windows $ viewOnScreen 0 "doc" )
                                                                                            >>  doShift "doc"
        -- Fontforge
        ,   className =? "fontforge"                                                        --> doShift' "ful"

        ,   (      className =? "fontforge"
            <&&> ( title     =? "FontForge"
            <||>   title     =? "Open Font"
            <||>   title     =? "Kerning"
            <||>   title     =? "Warnings"
            ))                                                                              --> myDoFloat 
        
        ]
        <+> namedScratchpadManageHook myScratchpads
        <+> manageDocks
        <+> manageSpawn
 
        where
        
            -- (?^)
            -- Returns True if x isPrefixOf q
            (?^) :: ( Eq a , Functor m ) => [ a ] -> m [ a ] -> m Bool
            x ?^ q = fmap ( x `isPrefixOf` ) q

            -- myDoFloat
            -- Custom managers for floating windows. 
            myDoFloat :: ManageHook
            myDoFloat' :: ManageHook
            myDoFloat = doRectFloat ( W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2) ) 
            myDoFloat' = doRectFloat ( W.RationalRect (1 % 16) (1 % 16) (7 % 8) (7 % 8) )
            
            -- doShift'
            -- Variation of doShift that switches to the workspace
            doShift' = doF . liftM2 (.) W.greedyView W.shift


myStartupHook :: X ()
myStartupHook =
        ( spawn $ "truncate -s 0 " ++ myLogFile )       -- clear log file
    >>  ( windows $ onlyOnScreen 0 "sys" )
    >>  ( windows $ onlyOnScreen 1 "vim" )
    >>  ( spawnOnOnce "vim" $ xappCommand' myEditor )
    >>  ( nextScreen )


myEventHook :: Event -> X All
myEventHook ev = 
        focusOnMouseMoveHook myFocusFollowsMouse
    <+> keyDownEventHook ( myKeyDownBindings myConfig )
    <+> keyUpEventHook   ( myKeyUpBindings   myConfig )
      $ ev
      where
        focusOnMouseMoveHook :: Bool -> Event -> X All
        focusOnMouseMoveHook True  = focusOnMouseMove
        focusOnMouseMoveHook False = mempty




---------- MAIN -----------
---------------------------

myConfig = def
    {
      terminal              = xappCommand myTerminal
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
