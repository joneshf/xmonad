import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName(setWMName)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import Graphics.X11.ExtraTypes.XF86

import qualified Data.Map as M

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { terminal          = "urxvt"
        , modMask           = mod4Mask
        , handleEventHook   = fullscreenEventHook
        , borderWidth       = 3
        , workspaces        = myWorkspaces
        , keys              = myKeys
        , manageHook        = manageDocks <+> manageHook defaultConfig
        , layoutHook        = avoidStruts  $  layoutHook defaultConfig
        , logHook           = dynamicLogWithPP xmobarPP
            { ppOutput          = hPutStrLn xmproc
            , ppTitle           = xmobarColor "green" "" . shorten 50
            , ppHiddenNoWindows = id
            }
        }

myWorkspaces = [ "1: IRC"
               , "2: Web"
               , "3: Terminal"
               , "4: Code"
               ] ++ map show [5..9]

myKeys k = M.union (M.fromList (moreKeys k)) (keys defaultConfig k)

moreKeys conf@(XConfig {XMonad.modMask = modm}) =
    [ -- These two lines allow java to work with non-reparenting.
      ((modm, xK_z),                    setWMName   "LG3D")
    , ((modm, xK_Z),                    setWMName   "XMonad")
    , ((0, xF86XK_AudioRaiseVolume),    spawn       "amixer -c 0 set Master 2dB+")
    , ((0, xF86XK_AudioLowerVolume),    spawn       "amixer -c 0 set Master 2dB-")
    , ((0, xF86XK_AudioMute),           spawn       "amixer sset Master toggle")
    ]
