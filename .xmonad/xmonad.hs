------------------------------------------------------------------------
-- Imports 
------------------------------------------------------------------------

import XMonad hiding ( (|||) )
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.Run(spawnPipe)
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
import XMonad.Actions.WithAll

import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M 

------------------------------------------------------------------------
-- Settings
------------------------------------------------------------------------

myModKey = mod4Mask
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]
myBorderWidth = 2
myNormalBorderColor  = "#282828"
myFocusedBorderColor = "#458588"
myTerminal = "alacritty"
mAppLauncher = "rofi -show drun -theme ~/.config/rofi/launcher.rasi"
myWebBrowser = "qutebrowser"

------------------------------------------------------------------------
-- Startup
------------------------------------------------------------------------

myStartupHook = do
    spawnOnce "picom --experimental-backends --backend glx &"
    spawnOnce "dunst &"
    spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &"
    spawnOnce "betterlockscreen -u ~/usr/share/backgrounds/gruvbox.jpg &"

------------------------------------------------------------------------
-- Key bindings
------------------------------------------------------------------------

myKeys = 

    -- Launching apps
    [ ("M-<Return>", spawn myTerminal)
    , ("M-d", spawn mAppLauncher)
    , ("M-w", spawn myWebBrowser)
    , ("M-u", spawn (myTerminal ++ " -e mocp"))
    , ("M-n", spawn (myTerminal ++ " -e pacmixer"))
    , ("M-s", spawn (myTerminal ++ " -e ranger"))
    , ("M-S-s", spawn "flameshot gui")

    -- Windows
    , ("M-f", sendMessage (Toggle FULL) >> sendMessage ToggleStruts)
    , ("M-S-q", kill)
    , ("M-S-a", killAll)
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-m", windows W.focusMaster)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-S-m", windows W.swapMaster)
    , ("M-S-h", sendMessage Shrink)
    , ("M-S-l", sendMessage Expand)
    , ("M-t", withFocused $ windows . W.sink)
    , ("M-,", sendMessage (IncMasterN (1)))
    , ("M-.", sendMessage (IncMasterN (-1)))
    , ("M-S-n", refresh) -- resize windows to correct size 

    -- Xmonad
    , ("M-S-r", spawn "xmonad --recompile; xmonad --restart")
    , ("M-x c", io (exitWith ExitSuccess)) 
    , ("M-x s", spawn "systemctl poweroff")
    , ("M-x r", spawn "systemctl reboot")
    , ("M-x l", spawn "betterlockscreen -l blur")

    -- Layouts 
    , ("M-<Tab>", sendMessage NextLayout)
    , ("M-S-<Tab>", sendMessage $ JumpToLayout "Tall")
    ]

------------------------------------------------------------------------
-- Mouse bindings
------------------------------------------------------------------------

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myClickJustFocuses :: Bool
myClickJustFocuses = False

mMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layout:
------------------------------------------------------------------------

myLayout = smartBorders
    . avoidStruts
    . mkToggle (NOBORDERS ?? FULL ?? EOT)
    . spacingRaw False (Border 10 10 10 10) True (Border 5 5 5 5) True 
    $ (tall ||| mirror ||| spiral (6/7) ||| mCol ||| threeCol ||| Grid)
        where  tall = Tall 1 (3/100) (1/2)
	       mCol = multiCol [1] 1 0.01 (-0.5)
	       mirror = Mirror (Tall 1 (3/100) (3/5))
	       threeCol = ThreeCol 1 (3/100) (1/2)

------------------------------------------------------------------------
-- Window rules:
------------------------------------------------------------------------

myManageHook = composeAll
    [ resource  =? "desktop_window" --> doIgnore ]

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks def {

        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModKey,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        mouseBindings      = mMouseBindings,
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook 

	} `additionalKeysP` myKeys

------------------------------------------------------------------------
-- Event handling
------------------------------------------------------------------------

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging
------------------------------------------------------------------------

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()
