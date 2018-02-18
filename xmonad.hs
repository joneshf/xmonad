import XMonad hiding (focus)
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseResize
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Layout
import XMonad.Layout.Decoration
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders (ConfigurableBorder, SetsAmbiguous(..), lessBorders, smartBorders)
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger
import XMonad.StackSet (Stack(..))
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad

import Data.List (delete)
import qualified Data.Map as M
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86

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
    , className =? "Pidgin"       --> doFloat
    , className =? "Lxappearance" --> doFloat
    , className =? "Wish" 	  --> doFloat
    , "Xchat"    `shiftClassTo` "1: Chat"
    , "Pidgin"   `shiftClassTo` "1: Chat"
    --, "Chromium" `shiftClassTo` "4: Web"
    , className =? "Chromium" <&&> fmap not isFullscreen --> doShift "4: Web"
    , appName =? "sun-awt-X11-XFramePeer" --> doFloat
    ]

myWorkspaces :: [String]
myWorkspaces = [ "1: Chat"
               , "2: Term"
               , "3: Code"
               , "4: Web"
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
    , ((modm,                     xF86XK_MonBrightnessUp),   spawn "xbacklight -inc 1")
    , ((modm,                     xF86XK_MonBrightnessDown), spawn "xbacklight -dec 1")
    , ((modm .|. shiftMask,       xF86XK_MonBrightnessUp),   spawn "xbacklight -inc 10")
    , ((modm .|. shiftMask,       xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
    -- Volume controls.
    , ((0,                        xF86XK_AudioLowerVolume), spawn "amixer -c 1 set Master 2dB-")
    , ((0,                        xF86XK_AudioRaiseVolume), spawn "amixer -c 1 set Master 2dB+")
    , ((0,                        xF86XK_AudioMute),        spawn "amixer sset Master toggle")
    , ((modm .|. controlMask .|. shiftMask, xK_t), namedScratchpadAction scratchpads "htop")
    -- dmenu.
    , ((modm,                     xK_p),            spawn "dmenu_run -fn \"xft:Droid Sans Mono\"")
    -- Screenshot.
    , ((mod1Mask .|. shiftMask, xK_3), spawn "import -window root ~/screenshots/screenshot-$(date +%s).png")
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
    hiddens _ _ Nothing xs = fst <$> init xs
    hiddens _ _ (Just (Stack {focus = x})) xs = delete x (fst <$> xs)
