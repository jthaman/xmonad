import XMonad

import XMonad.Util.Themes
import XMonad.Util.Ungrab
import XMonad.Util.EZConfig

import XMonad.Actions.DwmPromote
import XMonad.Actions.SpawnOn

import Graphics.X11.ExtraTypes.XF86

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog

import XMonad.Layout.Tabbed
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.Minimize

myConfig = def
    { modMask = mod4Mask
    , terminal = myTerminal
    , startupHook = myStartupHook
    , borderWidth = 1
    , layoutHook = myLayout
    }
    `additionalKeysP`
    [ ("M-<Return>" , dwmpromote)
    , ("M-S-l", spawn "systemctl suspend")
    , ("M-S-p", spawn "systemctl poweroff")
    , ("M-S-s", spawn "xfce4-screenshooter")
    , ("M-z" , spawn "thunar")
    ]
    `remapKeysP`
    [ ("M-S-<Backspace>", "M-S-q")
    , ("M-S-q", "M-S-c")
    ]

myTabConfig = def { activeColor = "#3d3d3d"
                  , inactiveColor = "#000000"
                  , urgentColor = "#FDF6E3"
                  , activeTextColor = "#ffffff"
                  , inactiveTextColor = "#ffffff"
                  , urgentTextColor = "#1ABC9C"
                  , fontName = "xft:Sans:size=10:antialias=true:style=bold"
                  , decoHeight = 24
                  }

myLayout =
  tiled
  ||| Mirror tiled
  ||| tabbed shrinkText myTabConfig
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 0.6    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes

myTerminal = "xfce4-terminal"

myStartupHook = do
  spawn "killall redshift"
  spawn "killall trayer"
  spawn "xsetroot -cursor_name left_ptr"
  spawn "pgrep trayer || trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --tint 0xffffff --height 24"
  spawn "feh --bg-scale ~/Pictures/Firefox_wallpaper.png"
  spawnOn "2" "pgrep firefox || firefox"
  spawnOn "3" "pgrep emacs || emacs"
  spawnOn "2" "pgrep keepassxc || keepassxc"
  spawnOn "5" "pgrep signal || signal-desktop"
  spawn "mullvad connect"
  spawn "mullvad"
  spawn "dunst"
  spawn "pgrep redshift || redshift -t 3000:3000 -l 38.90:-77.03"
  spawn "lxpolkit"
  spawn "syncthing --no-browser"
  spawn "pgrep nm-applet || nm-applet"
  spawn "pgrep xfce4-clipman || xfce4-clipman"
  spawn "pgrep volumeicon || volumeicon"
  spawn "pgrep xfce4-power-manager || xfce4-power-manager"
  spawn "xss-lock -- i3lock -e -c 000000"
  spawn "numlockx on"
  spawn "setxkbmap -option 'ctrl:swap_lalt_lctl,caps:escape'"
  spawn "xset r rate 300 40"
  spawn "xinput set-prop \"DELL081A:00 044E:120A Touchpad\" \"libinput Tapping Enabled\" 1"
  spawn "xinput set-prop \"SynPS/2 Synaptics TouchPad\" \"libinput Tapping Enabled\" 1"
  spawn "xinput set-prop \"DELL081C:00 044E:121F Touchpad\" \"libinput Tapping Enabled\" 1"

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar ~/.xmonad/xmobarrc" (pure def)) defToggleStrutsKey
     $ myConfig
