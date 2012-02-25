import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import Graphics.X11.ExtraTypes.XF86
import Data.Map(fromList)
import Data.Monoid(mappend)

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

myKeys = keys defaultConfig `mappend`
            \c -> fromList
            [ ((0, xF86XK_AudioRaiseVolume),    spawn "amixer -c 0 set Master 2dB+")
            , ((0, xF86XK_AudioLowerVolume),    spawn "amixer -c 0 set Master 2dB-")
            , ((0, xF86XK_AudioMute),           spawn "amixer sset Master toggle")
            ]
