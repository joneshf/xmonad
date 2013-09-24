import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName(setWMName)
import XMonad.Util.Run(spawnPipe)
import System.IO

import qualified Data.Map as M

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { terminal          = "gnome-terminal"
        , modMask           = mod4Mask
        , handleEventHook   = fullscreenEventHook
        , borderWidth       = 2
        , workspaces        = myWorkspaces
        , keys              = myKeys
        , manageHook        = manageDocks <+> manageHook defaultConfig
        , layoutHook        = avoidStruts  $  layoutHook defaultConfig
        , logHook           = dynamicLogWithPP xmobarPP
            { ppOutput          = hPutStrLn xmproc
            , ppTitle           = xmobarColor "green" ""
            , ppHiddenNoWindows = id
            }
        }

myWorkspaces = [ "1: Chat"
               , "2: Term"
               , "3: Code"
               , "4: Firefox"
               , "5: Chromium"
               ] ++ map show [6..9 :: Int]

myKeys k = M.union (M.fromList (moreKeys k)) (keys defaultConfig k)

moreKeys conf@(XConfig {XMonad.modMask = modm}) =
    [ -- These two lines allow java to work with non-reparenting.
      ((modm, xK_z),                        setWMName   "LG3D")
    , ((modm, xK_Z),                        setWMName   "XMonad")
    , ((modm, xK_Left),                     prevWS)
    , ((modm, xK_Right),                    nextWS)
    , ((modm .|. shiftMask, xK_Left),       shiftToPrev >> prevWS)
    , ((modm .|. shiftMask, xK_Right),      shiftToNext >> nextWS)
    , ((mod1Mask .|. controlMask, xK_l),    spawn       "xscreensaver-command -lock")
    , ((modm, xK_KP_Add),                   spawn       "amixer -c 1 set Master 2dB+")
    , ((modm, xK_KP_Subtract),              spawn       "amixer -c 1 set Master 2dB-")
    , ((modm, xK_KP_0),                     spawn       "amixer -c 1 set Master 0%")
    , ((modm, xK_KP_1),                     spawn       "amixer -c 1 set Master 10%")
    , ((modm, xK_KP_2),                     spawn       "amixer -c 1 set Master 20%")
    , ((modm, xK_KP_3),                     spawn       "amixer -c 1 set Master 30%")
    , ((modm, xK_KP_4),                     spawn       "amixer -c 1 set Master 40%")
    , ((modm, xK_KP_5),                     spawn       "amixer -c 1 set Master 50%")
    , ((modm, xK_KP_6),                     spawn       "amixer -c 1 set Master 60%")
    , ((modm, xK_KP_7),                     spawn       "amixer -c 1 set Master 70%")
    , ((modm, xK_KP_8),                     spawn       "amixer -c 1 set Master 80%")
    , ((modm, xK_KP_8),                     spawn       "amixer -c 1 set Master 80%")
    , ((modm, xK_KP_9),                     spawn       "amixer -c 1 set Master 90%")
    , ((modm, xK_KP_Enter),                 spawn       "amixer sset Master toggle")
    ]
