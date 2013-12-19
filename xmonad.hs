import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Util.Run

import qualified Data.Map as M
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { terminal          = "xfce4-terminal"
        , modMask           = mod4Mask
        , handleEventHook   = fullscreenEventHook
        , borderWidth       = 2
        , workspaces        = myWorkspaces
        , keys              = myKeys
        , manageHook        = myManageHook <+> manageDocks <+> manageHook defaultConfig
        , layoutHook        = avoidStruts $ layoutHook defaultConfig
        , logHook           = dynamicLogWithPP xmobarPP
            { ppOutput          = hPutStrLn xmproc
            , ppTitle           = xmobarColor "green" ""
            , ppHiddenNoWindows = id
            }
        }

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , isDialog     --> doFloat
    ]

myWorkspaces :: [String]
myWorkspaces = [ "1: Chat"
               , "2: Term"
               , "3: Code"
               , "4: Chromium"
               ] ++ map show [5..9 :: Int]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys k = M.fromList (moreKeys k) `M.union` keys defaultConfig k

moreKeys :: XConfig t -> [((KeyMask, KeySym), X ())]
moreKeys (XConfig {XMonad.modMask = modm}) =
    -- These two lines allow java to work with non-reparenting.
    [ ((modm,                     xK_z),            setWMName "LG3D")
    , ((modm,                     xK_Z),            setWMName "XMonad")
    -- Workspace movement.
    , ((modm,                     xK_Left),         prevWS)
    , ((modm,                     xK_Right),        nextWS)
    , ((modm     .|. shiftMask,   xK_Left),         shiftToPrev >> prevWS)
    , ((modm     .|. shiftMask,   xK_Right),        shiftToNext >> nextWS)
    -- Lock the screen.
    , ((mod1Mask .|. controlMask, xK_l),            spawn "xscreensaver-command -lock")
    -- Brightness.
    , ((modm,                     xF86XK_MonBrightnessUp),   spawn "xbacklight -inc 10")
    , ((modm,                     xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
    -- Volume controls.
    , ((0,                        xF86XK_AudioLowerVolume), spawn "amixer -c 1 set Master 2dB-")
    , ((0,                        xF86XK_AudioRaiseVolume), spawn "amixer -c 1 set Master 2dB+")
    , ((0,                        xF86XK_AudioMute),        spawn "amixer sset Master toggle")
    ]
