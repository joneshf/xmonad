{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}

import "base" Data.List   (delete)
import "base" Data.Monoid (Endo, (<>))

import "X11" Graphics.X11.ExtraTypes.XF86
    ( xF86XK_AudioLowerVolume
    , xF86XK_AudioMute
    , xF86XK_AudioRaiseVolume
    , xF86XK_MonBrightnessDown
    , xF86XK_MonBrightnessUp
    )
import "xmonad" XMonad                               hiding (focus)
import "xmonad-contrib" XMonad.Actions.CycleWS
    ( nextWS
    , prevWS
    , shiftToNext
    , shiftToPrev
    )
import "xmonad-contrib" XMonad.Actions.MouseResize   (MouseResize)
import "xmonad-contrib" XMonad.Hooks.DynamicLog
    ( dynamicLogWithPP
    , ppHiddenNoWindows
    , ppOutput
    , ppTitle
    , xmobarColor
    , xmobarPP
    )
import "xmonad-contrib" XMonad.Hooks.EwmhDesktops    (fullscreenEventHook)
import "xmonad-contrib" XMonad.Hooks.ManageDocks
    ( avoidStruts
    , docksEventHook
    , manageDocks
    )
import "xmonad-contrib" XMonad.Hooks.ManageHelpers
    ( doFullFloat
    , isDialog
    , isFullscreen
    )
import "xmonad-contrib" XMonad.Hooks.SetWMName       (setWMName)
import "xmonad-contrib" XMonad.Layout.Decoration
    ( Decoration
    , DefaultShrinker
    )
import "xmonad-contrib" XMonad.Layout.LayoutModifier (ModifiedLayout)
import "xmonad-contrib" XMonad.Layout.NoBorders
    ( ConfigurableBorder
    , SetsAmbiguous(..)
    , lessBorders
    , smartBorders
    )
import "xmonad-contrib" XMonad.Layout.SimpleFloat
    ( SimpleDecoration
    , SimpleFloat
    , simpleFloat
    )
import "xmonad-contrib" XMonad.Layout.Spacing
    ( SmartSpacingWithEdge
    , smartSpacingWithEdge
    )
import "xmonad-contrib" XMonad.Layout.WindowArranger (WindowArranger)
import "xmonad" XMonad.StackSet                      (Stack(Stack, focus))
import "xmonad-contrib" XMonad.Util.NamedScratchpad
    ( NamedScratchpad(NS)
    , defaultFloating
    , namedScratchpadAction
    , namedScratchpadManageHook
    )
import "xmonad-contrib" XMonad.Util.Run              (hPutStrLn, spawnPipe)

import qualified "containers" Data.Map as M

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { terminal          = "xfce4-terminal"
        , modMask           = mod4Mask
        , handleEventHook   = fullscreenEventHook <> docksEventHook
        , borderWidth       = 1
        , focusedBorderColor = "gray"
        , workspaces        = myWorkspaces
        , keys              = myKeys
        , manageHook        = namedScratchpadManageHook scratchpads <+> myManageHook <+> manageDocks <+> manageHook defaultConfig
        , layoutHook        = smartBorders $ avoidStruts myLayoutHook
        , logHook           = dynamicLogWithPP xmobarPP
            { ppOutput          = hPutStrLn xmproc
            , ppTitle           = xmobarColor "green" ""
            , ppHiddenNoWindows = id
            }
        }

scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "htop" "xterm -e htop" (title =? "htop") defaultFloating
    ]

shiftClassTo :: String -> WorkspaceId -> Query (Endo WindowSet)
shiftClassTo s ws = className =? s --> doShift ws

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ isFullscreen                --> doFullFloat
    , isDialog                    --> doFloat
    , className =? "Lxappearance" --> doFloat
    , className =? "Wish" 	  --> doFloat
    , "Xchat"    `shiftClassTo` "1: Chat"
    , className =? "Chromium" <&&> fmap not isFullscreen --> doShift "4: Web"
    , appName =? "sun-awt-X11-XFramePeer" --> doFloat
    ]

myWorkspaces :: [String]
myWorkspaces =
    [ "1: Chat"
    , "2: Term"
    , "3: Code"
    , "4: Web"
    , "5"
    , "6"
    , "7"
    , "8"
    , "9"
    ]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys =
    javaNonReparentingKeys
        <> workspaceMovementKeys
        <> const lockScreenKeys
        <> brightnessKeys
        <> volumeKeys
        <> dmenuKeys
        <> const screenshotKeys
        <> keys defaultConfig

javaNonReparentingKeys :: XConfig a -> M.Map (KeyMask, KeySym) (X ())
javaNonReparentingKeys XConfig { modMask } = M.fromList
    [ ((modMask, xK_z), setWMName "LG3D")
    , ((modMask, xK_Z), setWMName "XMonad")
    ]

workspaceMovementKeys :: XConfig a -> M.Map (KeyMask, KeySym) (X ())
workspaceMovementKeys XConfig { modMask } = M.fromList
    [ ((modMask, xK_Left), prevWS)
    , ((modMask, xK_Right), nextWS)
    , ((modMask .|. shiftMask, xK_Left), shiftToPrev >> prevWS)
    , ((modMask .|. shiftMask, xK_Right), shiftToNext >> nextWS)
    ]

lockScreenKeys :: M.Map (KeyMask, KeySym) (X ())
lockScreenKeys = M.fromList
    [ ((mod1Mask .|. controlMask, xK_l), spawn "xscreensaver-command -lock")
    ]

brightnessKeys :: XConfig a -> M.Map (KeyMask, KeySym) (X ())
brightnessKeys XConfig { modMask } = M.fromList
    [ ((modMask, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 1")
    , ((modMask, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 1")
    , ((modMask .|. shiftMask, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
    , ((modMask .|. shiftMask, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
    ]

volumeKeys :: XConfig a -> M.Map (KeyMask, KeySym) (X ())
volumeKeys XConfig { modMask } = M.fromList
    [ ((0, xF86XK_AudioLowerVolume), spawn "amixer -c 1 set Master 2dB-")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -c 1 set Master 2dB+")
    , ((0, xF86XK_AudioMute), spawn "amixer sset Master toggle")
    , ((modMask .|. controlMask .|. shiftMask, xK_t), namedScratchpadAction scratchpads "htop")
    ]

dmenuKeys :: XConfig a -> M.Map (KeyMask, KeySym) (X ())
dmenuKeys XConfig { modMask } = M.fromList
    [ ((modMask, xK_p), spawn "dmenu_run -fn \"xft:Droid Sans Mono\"")
    ]

screenshotKeys :: M.Map (KeyMask, KeySym) (X ())
screenshotKeys = M.fromList
    [ ((mod1Mask .|. shiftMask, xK_3), spawn "import -window root ~/screenshots/screenshot-$(date +%s).png")
    , ((mod1Mask .|. shiftMask, xK_4), spawn "import ~/screenshots/screenshot-$(date +%s).png")
    ]

myLayoutHook ::
    ModifiedLayout
        (ConfigurableBorder MyAmbiguity)
        (ModifiedLayout
            SmartSpacingWithEdge
            (Choose
                (Choose Tall (Choose (Mirror Tall) Full))
                (ModifiedLayout
                    (Decoration SimpleDecoration DefaultShrinker)
                    (ModifiedLayout
                        MouseResize
                        (ModifiedLayout WindowArranger SimpleFloat)))))
        Window
myLayoutHook = lessBorders MyAmbiguity $ smartSpacingWithEdge 4 $ layoutHook defaultConfig ||| simpleFloat

data MyAmbiguity = MyAmbiguity deriving (Read, Show)

instance SetsAmbiguous MyAmbiguity where
    hiddens _ _ Nothing xs                    = fst <$> init xs
    hiddens _ _ (Just (Stack {focus = x})) xs = delete x (fst <$> xs)
