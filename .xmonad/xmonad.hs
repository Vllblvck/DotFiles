------------------------------------------------------------------------
-- Imports 
------------------------------------------------------------------------

import XMonad hiding ( (|||) )

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad

import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.MultiColumns
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Actions.WithAll

import Data.Monoid
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map        as M 

------------------------------------------------------------------------
-- Settings
------------------------------------------------------------------------

myNormalBorderColor  = "#2E3440"
myFocusedBorderColor = "#81A1C1"
currentWorkspaceColor = "#81A1C1"
hiddenWorkspaceColor = "#D8DEE9"
visibleWorkspaceColor = "#4C566A"
urgentWorkspaceColor = "#BF616A"
windowTitleColor = "#D8DEE9" 
layoutNameColor = "#D8DEE9"
stdinSeparatorColor = "#A3BE8C"

myModKey = mod4Mask
myBorderWidth = 2
myTerminal = "alacritty"
myWebBrowser = "qutebrowser"

------------------------------------------------------------------------
-- Startup
------------------------------------------------------------------------

myStartupHook = do
    setWMName "LG3D" -- This exists so java apps display properly
    spawnOnce "picom --experimental-backends --backend glx &"
    spawnOnce "dunst &"
    spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &"
    spawnOnce "betterlockscreen -u /usr/share/backgrounds/mountains_with_galaxy.jpg &"

------------------------------------------------------------------------
-- Key bindings
------------------------------------------------------------------------

myKeys = 

    -- Xmonad
    [ ("M-S-r", spawn "xmonad --recompile; xmonad --restart")
    , ("M-x c", io (exitWith ExitSuccess)) 
    , ("M-x s", spawn "systemctl poweroff")
    , ("M-x r", spawn "systemctl reboot")
    , ("M-x l", spawn "betterlockscreen -l")

    -- Windows
    , ("M-S-q", kill)
    , ("M-S-a", killAll)
    , ("M-h", sendMessage Shrink)
    , ("M-l", sendMessage Expand)
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-m", windows W.focusMaster)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-S-m", windows W.swapMaster)
    , ("M-t", withFocused $ windows . W.sink)
    , ("M-,", sendMessage (IncMasterN (1)))
    , ("M-.", sendMessage (IncMasterN (-1)))

    -- Layouts 
    , ("M-<Tab>", sendMessage NextLayout)
    , ("M-S-<Tab>", sendMessage $ JumpToLayout "Tall")
    , ("M-f", sendMessage (Toggle FULL) >> sendMessage ToggleStruts)
    , ("M-v", sendMessage $ Toggle MIRROR)

    -- Scratchpads
    , ("M-u", namedScratchpadAction myScratchpads "mocp")
    , ("M-S-<Return>", namedScratchpadAction myScratchpads "terminal")

    -- Launching apps
    , ("M-<Return>", spawn myTerminal)
    , ("M-d", shellPrompt shellXPConfig)
    , ("M-w", spawn myWebBrowser)
    , ("M-n", spawn (myTerminal ++ " -e pacmixer"))
    , ("M-s", spawn (myTerminal ++ " -e ranger"))
    , ("M-S-s", spawn "flameshot gui")
    , ("M-S-u", spawn (myTerminal ++ " -t music -e mocp")) ]

------------------------------------------------------------------------
-- Scratchpads
------------------------------------------------------------------------

myScratchpads = [ NS "mocp" (myTerminal ++ " -t mocp -e mocp") (title =? "mocp") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
                , NS "terminal" (myTerminal ++ " -t terminal") (title =? "terminal") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)) 
		]

------------------------------------------------------------------------
-- Layouts
------------------------------------------------------------------------

myLayout =
    smartBorders
    . avoidStruts
    . mkToggle (NOBORDERS ?? FULL ?? EOT)
    . mkToggle (single MIRROR)
    . spacingRaw False (Border 10 10 10 10) True (Border 5 5 5 5) True 
    $ (tall ||| spiral (6/7) ||| mCol ||| threeCol ||| Grid)
        where  tall = Tall 1 (3/100) (1/2)
	       mCol = multiCol [1] 1 0.01 (-0.5)
	       threeCol = ThreeCol 1 (3/100) (1/2)

------------------------------------------------------------------------
-- Window rules:
------------------------------------------------------------------------

myManageHook = composeAll
    [ namedScratchpadManageHook myScratchpads
    , className =? "qutebrowser"     --> doShift ( myWorkspaces !! 0)
    , className =? "LBRY"            --> doShift ( myWorkspaces !! 0)
    , className =? "firefox"         --> doShift ( myWorkspaces !! 0)
    , className =? "Brave-browser"   --> doShift ( myWorkspaces !! 0)
    , className =? "code-oss"        --> doShift ( myWorkspaces !! 2)
    , className =? "jetbrains-rider" --> doShift ( myWorkspaces !! 2)
    , title     =? "music"           --> doShift ( myWorkspaces !! 3)
    , className =? "Caprine"         --> doShift ( myWorkspaces !! 4)
    , className =? "discord"         --> doShift ( myWorkspaces !! 4)
    , className =? "Thunderbird"     --> doShift ( myWorkspaces !! 5)
    , className =? "qBittorrent"     --> doShift ( myWorkspaces !! 6)
    , className =? "Virt-manager"    --> doShift ( myWorkspaces !! 7)
    , resource  =? "desktop_window"  --> doIgnore
    , insertPosition End Newer ]

------------------------------------------------------------------------
-- Xprompt
------------------------------------------------------------------------

shellXPConfig :: XPConfig
shellXPConfig = def
      { font                = "xft:Mononoki Nerd Font:size=13"
      , bgColor             = "#2E3440"
      , fgColor             = "#D8DEE9"
      , bgHLight            = "#81A1C1"
      , fgHLight            = "#D8DEE9"
      , promptBorderWidth   = 0
      , position            = Top
      , defaultText         = []
      , showCompletionOnTab = False
      , alwaysHighlight     = True
      , maxComplRows        = Just 5
      }

------------------------------------------------------------------------
-- Workspaces
------------------------------------------------------------------------

xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]
        
myWorkspaces = clickable . (map xmobarEscape) 
               $ ["WEB","MISC","DEV","MUS","CHAT","MAIL","TRNT","VIRT"]
  where
        clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..8] l,
                      let n = i ] 

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks def 
        {
        terminal           = myTerminal,
        modMask            = myModKey,
        workspaces         = myWorkspaces,
        borderWidth        = myBorderWidth,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        layoutHook         = myLayout,
        startupHook        = myStartupHook,
        manageHook         = myManageHook,
        logHook            = dynamicLogWithPP xmobarPP
		  		 { ppOutput = hPutStrLn xmproc
		  		 , ppCurrent = xmobarColor currentWorkspaceColor "" . wrap "[" "]"
		  		 , ppVisible = xmobarColor visibleWorkspaceColor ""
				 , ppHidden = xmobarColor hiddenWorkspaceColor ""
				 , ppHiddenNoWindows = xmobarColor visibleWorkspaceColor ""
				 , ppLayout = xmobarColor layoutNameColor ""
				 , ppTitle = xmobarColor windowTitleColor "" . shorten 40
				 , ppSep = "<fc=" ++ stdinSeparatorColor ++ "> || </fc>"
				 , ppUrgent = xmobarColor urgentWorkspaceColor "" 
		  		 },
        handleEventHook    = mempty,
        focusFollowsMouse  = True,
        clickJustFocuses   = False
	} `additionalKeysP` myKeys
