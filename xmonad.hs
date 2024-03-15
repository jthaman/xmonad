import XMonad

import XMonad.Util.Themes
import XMonad.Util.Ungrab
import XMonad.Util.EZConfig

import System.Exit
import XMonad.Prompt.ConfirmPrompt

import XMonad.Actions.DwmPromote
import XMonad.Actions.SpawnOn
import XMonad.Actions.Minimize
import XMonad.Actions.CycleWS

import Graphics.X11.ExtraTypes.XF86

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ScreenCorners

import XMonad.Layout.Minimize
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import qualified XMonad.Layout.BoringWindows as BW


myConfig = def
    { modMask = mod1Mask
    , terminal = "xfce4-terminal"
    , startupHook = myStartupHook
    , borderWidth = 5
    , normalBorderColor = "#dfd5cf"
    , focusedBorderColor = "#c9b9b0"
    , layoutHook = myLayout
    , manageHook = manageSpawn
    , handleEventHook = myEventHook
    }
    `additionalKeysP`
    [ ("M-<Return>" , dwmpromote)
    , ("M-S-q", confirmPrompt def "exit" $ io (exitWith ExitSuccess))
    , ("M-S-l", spawn "systemctl suspend")
    , ("M-S-p", spawn "systemctl poweroff")
    , ("M-S-r", spawn "systemctl reboot")
    , ("M-S-s", spawn "xfce4-screenshooter")
    , ("M-z", spawn "nemo")
    , ("M-p", spawn "rofi -show drun") -- use rofi for showing flatpaks
    , ("M-S-<Return>", spawn "gnome-terminal")
    , ("M-x", spawn "catfish")
    , ("M-m", withFocused minimizeWindow)
    , ("M-S-m", withLastMinimized maximizeWindowAndFocus)
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +5%")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -5%")
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle")
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    ]

myLayout =
  screenCornerLayoutHook
  $ minimize . BW.boringWindows
  $ spacingRaw False (Border 10 5 10 5) True (Border 5 10 5 10) True
  $ gaps [(U,5), (R,150), (L,150), (D,5)]
  $ tiled
  ||| Mirror tiled
  ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 0.6    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes

myStartupHook = do
  -- GUI Programs
  -- spawnOn "1" "pgrep thunderbird || thunderbird"
  spawnOn "2" "pgrep keepassxc || keepassxc"
  spawnOn "2" "pgrep firefox || firefox"
  spawnOn "3" "pgrep emacs || emacs"
--  spawnOn "4" "pgrep rhythmbox || rhythmbox"
  spawnOn "5" "pgrep signal || signal-desktop"
  -- Programs
  spawn "killall redshift"
  spawn "xsetroot -cursor_name left_ptr"
  spawn "feh --bg-scale /run/media/john/External Drive/Sync/Documents/pic.jpg"
  spawn "mullvad connect"
  spawn "mullvad"
  -- spawn "dunst"
  spawn "pgrep redshift || redshift -t 4000:4000 -l 38.90:-77.03"
  spawn "lxpolkit"
  spawn "syncthing --no-browser"
  spawn "pgrep nm-applet || nm-applet"
  spawn "pgrep xfce4-clipman || xfce4-clipman"
  spawn "pgrep volumeicon || volumeicon"
  spawn "xss-lock -l -- xsecurelock"
  spawn "numlockx on"
  spawn "setxkbmap -option 'caps:escape'"
  spawn "xset r rate 300 40"
  addScreenCorners [ (SCUpperRight, moveTo Next (Not emptyWS))
                     , (SCUpperLeft,  moveTo Prev (Not emptyWS))
                     ]

myEventHook = screenCornerEventHook

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "tint2" (pure def)) defToggleStrutsKey
     $ myConfig
