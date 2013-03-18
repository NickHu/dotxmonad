import XMonad hiding (Tall)

import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow
import XMonad.Actions.PhysicalScreens

import XMonad.Prompt
import XMonad.Prompt.RunOrRaise

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.NoBorders
import XMonad.Layout.HintedTile

import DBus.Client
import System.Taffybar.XMonadLog

import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- Solarized colours
import Solarized
 
layouts = hintedTile Tall ||| hintedTile Wide ||| Full
  where
    hintedTile   = HintedTile nmaster delta ratio TopLeft
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

customXPConfig = defaultXPConfig                                    
  {
    font  = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u" 
  , fgColor = solarizedBlue
  , bgColor = solarizedBase02
  , fgHLight    = solarizedOrange
  , bgHLight    = solarizedBase02
  , promptBorderWidth = 0
  , position = Top
  , height   = 16
  }

manager = composeAll
  [
    isFullscreen                       --> (doF W.focusDown <+> doFullFloat)
  , className =? "Firefox"             --> (liftX (addWorkspace "www") >> doShift "www")
  , className =? "Gvim"                --> (liftX (addWorkspace "code") >> doShift "code")
  , className =? "URxvt"               --> (liftX (addWorkspace "sys") >> doShift "sys")
  , className =? "Deluge"              --> (liftX (addWorkspace "torrent") >> doShift "sys")
  ]

main = do
  bar <- spawn "taffybar"
  client <- connectSession
  let pp = taffybarPP {
    ppCurrent = taffybarColor solarizedMagenta ""
  , ppVisible = taffybarColor solarizedGreen ""
  , ppHidden = taffybarColor solarizedBase01 ""
  , ppHiddenNoWindows = taffybarColor solarizedBase00 ""
  , ppUrgent = taffybarColor solarizedOrange "" . wrap "!" "!"
  , ppTitle = taffybarColor solarizedBlue ""
  , ppLayout = taffybarColor solarizedViolet ""
  , ppSep = " <span foreground='" ++ solarizedCyan ++ "'>|</span> "
  }
  xmonad . ewmh $ withUrgencyHookC (BorderUrgencyHook solarizedOrange) urgencyConfig { suppressWhen = Focused, remindWhen = Dont } $ defaultConfig
    {
      terminal        = "urxvtc"
    , modMask         = mod4Mask
    , workspaces      = ["sys", "www"]
    , borderWidth     = 1
    , focusedBorderColor     = solarizedRed
    , normalBorderColor      = solarizedBase01

    , keys            = \c -> keyBindings c `M.union` keys defaultConfig c

    , layoutHook      = avoidStruts $ smartBorders $ layouts
    , manageHook      = manager <+> insertPosition Below Newer <+> manageDocks
    , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook <+> docksEventHook
    , logHook         = dbusLogWithPP client pp
    }
  where
    keyBindings (XConfig {modMask = modm}) = M.fromList $
      [ 
      -- Run or raise
        ((modm, xK_p                    ), runOrRaisePrompt customXPConfig)

      -- Workspaces
      , ((modm .|. shiftMask, xK_BackSpace), removeWorkspace)
      , ((modm .|. shiftMask, xK_v      ), selectWorkspace customXPConfig)
      , ((modm, xK_i                    ), withWorkspace customXPConfig (windows . W.shift))
      , ((modm .|. shiftMask, xK_i      ), withWorkspace customXPConfig (windows . copy))
      , ((modm, xK_o                    ), addWorkspacePrompt customXPConfig)
      , ((modm .|. shiftMask, xK_r      ), renameWorkspace customXPConfig)

      -- MPC
      , ((mod1Mask          , xK_comma ), spawn "dbus-send --type=method_call --dest=org.mpris.MediaPlayer2.mpd /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
      , ((mod1Mask          , xK_period), spawn "dbus-send --type=method_call --dest=org.mpris.MediaPlayer2.mpd /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
      , ((mod1Mask          , xK_p     ), spawn "dbus-send --type=method_call --dest=org.mpris.MediaPlayer2.mpd /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
      , ((mod1Mask          , xK_r     ), spawn "mpc random")
      ]
 
      -- mod-[1..9]       %! Switch to workspace N
      -- mod-shift-[1..9] %! Move client to workspace N
      ++
      zip (zip (repeat (modm)) [xK_1..xK_9]) (map (removeEmptyWorkspaceAfter . withNthWorkspace W.greedyView) [0..])
      ++
      zip (zip (repeat (modm .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])

      -- fix buggy screen order
      ++
      [((modm .|. mask, key), f sc)
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]
