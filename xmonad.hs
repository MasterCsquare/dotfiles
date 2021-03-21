import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad
import System.IO

yellow = "#ebbf83"
red = "#d95468"
blue = "#55ccff"
grey = "#384551"

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh $ docks def
        { handleEventHook = handleEventHook def <+> fullscreenEventHook
        , layoutHook = avoidStruts $ smartBorders $ layoutHook def
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor blue "" . shorten 80
                        , ppCurrent = xmobarColor yellow "" . wrap "[" "]"
                        , ppUrgent  = xmobarColor red yellow
                        }
        , manageHook = manageHook def <+> scratchpadManageHookDefault
        , modMask = mod4Mask
        , normalBorderColor  = grey
        , focusedBorderColor = red
        , terminal = "kitty"
        } `additionalKeysP`
        [ ("S-<Print>", spawn "maim -s ~/$(date +%s).png")
        , ("<Print>", spawn "maim ~/$(date +%s).png")
        , ("M-<Page_Up>", spawn "amixer set Master 3%+")
        , ("M-<Page_Down>", spawn "amixer set Master 3%-")
        , ("M-p", spawn "rofi -show run")
        , ("M-g", goToSelected def)
        , ("M-i", runOrRaise "emacs" (className =? "Emacs"))
        , ("M-f", runOrRaise "firefox" (className =? "firefox"))
        , ("M-s", scratchpadSpawnActionCustom "kitty --name=scratchpad")
        , ("M-b", sendMessage ToggleStruts)
        ]
