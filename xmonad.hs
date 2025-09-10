module Main where

import qualified Codec.Binary.UTF8.String as UTF8
import qualified Colors as C
import Control.Exception.Base
import qualified DBus as D
import qualified DBus.Client as D
import qualified DBus.Notify as N
import qualified Data.Map as M
import Data.Semigroup (All)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import XMonad
import XMonad.Actions.CopyWindow (copy, copyToAll, killAllOtherCopies)
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Minimize
import XMonad.Actions.Navigation2D
import XMonad.Actions.UpdateFocus
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.Place
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import qualified XMonad.Layout.BinarySpacePartition as BSP
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.Column
import XMonad.Layout.Combo
import XMonad.Layout.Dishes
import XMonad.Layout.DragPane
import XMonad.Layout.FocusTracking
import XMonad.Layout.Fullscreen
import XMonad.Layout.IfMax
import XMonad.Layout.Minimize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.StackTile
import XMonad.Layout.Tabbed
import XMonad.Layout.TrackFloating
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.StackSet (lookupWorkspace)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

reload :: X ()
reload = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

sendNotif :: N.Client -> String -> IO ()
sendNotif client str = do
  let note = N.blankNote {N.summary = "XMonad", N.body = Just $ N.Text str}
  N.notify client note
  return ()

myModMask = mod4Mask

myStartupHook = do
  spawnOnce "picom -b"
  spawnOnce "xsetroot -cursor_name left_ptr"

myLayoutHook =
  focusTracking $
    smartBorders $
      avoidStruts $
        BW.boringWindows $
          mkToggle (single MIRROR) $
            bsp ||| twoPane ||| full
  where
    bsp =
      named "BSP" $
        myMods BSP.emptyBSP
    twoPane =
      named "TwoPane" $
        myMods $
          TwoPane delta ratio
    tall =
      named "Tall" $
        myMods $
          Tall nmaster delta ratio
    full = named "Full" Full

    myMods = smartSpacingWithEdge 10 . minimize
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

myManageHook :: ManageHook
myManageHook =
  placeHook (smart (0.5, 0.5))
    <> composedManageHook
    <> manageHook def
  where
    cShifts =
      [ ("firefox", "web"),
        ("steam", "games"),
        ("Lutris", "games"),
        ("discord", "games"),
        ("REAPER", "music"),
        ("com.bitwig.BitwigStudio", "music"),
        ("Qtractor", "music"),
        ("Spotify", "music")
      ]

    cFloats =
      [ "Localsend",
        "pavucontrol",
        "Anki",
        "Pcmanfm",
        "Pcmanfm-qt",
        "Alacritty",
        "Thunar",
        "zenity",
        "kitty",
        "org.wezfurlong.wezterm"
      ]

    isXfce = className ^? "Xfce4"
    isLxqt = className ^? "lxqt"
    isDesktop = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DESKTOP"

    composedManageHook =
      composeOne . concat $
        [ [title =? "Picture-in-Picture" -?> doFloat],
          [className =? c -?> doF (W.shift w) | (c, w) <- cShifts],
          [className =? c -?> doFloat | c <- cFloats],
          [isDesktop -?> doLower <+> doIgnore],
          [checkDock -?> doRaise <+> doIgnore],
          [isNotification -?> doRaise <+> doIgnore],
          [isDialog -?> doFloat],
          [isXfce -?> doFloat],
          [isLxqt -?> doFloat],
          [willFloat -?> doFloat],
          [transience]
        ]

myPP :: D.Client -> PP
myPP dbus =
  def
    { ppOrder = \(_ : l : _ : _) -> [l],
      ppOutput = dbusOutput dbus
    }

myHandleEventHook :: Event -> X All
myHandleEventHook = minimizeEventHook

addKeysP :: N.Client -> [(String, X ())]
addKeysP client =
  [ ("M-q", io (sendNotif client "Reloading XMonad") >> reload),
    ("M-w", kill),
    ("M-=", appendWorkspacePrompt def),
    ("M--", removeWorkspace),
    ("M-s", windows copyToAll),
    ("M-S-s", killAllOtherCopies),
    ("M-b", sendMessage ToggleStruts >> toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled),
    -- ("M-<Space>", sendMessage NextLayout >> (dynamicLogString myPP >>= io . sendNotif client)),
    -- ("M-S-<Space>", sendMessage FirstLayout >> (dynamicLogString myPP >>= io . sendNotif client)),
    -- ("M-C-<Space>", sendMessage (Toggle MIRROR) >> (dynamicLogString myPP >>= io . sendNotif client)),
    -- movement
    ("M-<Tab>", nextWS),
    ("M-S-<Tab>", prevWS),
    ("M-C-<Tab>", toggleWS),
    ("M-j", BW.focusDown),
    ("M-k", BW.focusUp),
    ("M-m", BW.focusMaster),
    ("M-S-j", BW.swapDown),
    ("M-S-k", BW.swapUp),
    ("M-C-j", sendMessage $ BSP.ExpandTowards D),
    ("M-C-k", sendMessage $ BSP.ExpandTowards U),
    ("M-C-h", sendMessage $ BSP.ExpandTowards L),
    ("M-C-l", sendMessage $ BSP.ExpandTowards R),
    ("M-C-S-j", sendMessage $ BSP.ShrinkFrom D),
    ("M-C-S-k", sendMessage $ BSP.ShrinkFrom U),
    ("M-C-S-h", sendMessage $ BSP.ShrinkFrom L),
    ("M-C-S-l", sendMessage $ BSP.ShrinkFrom R),
    ("M-o", sendMessage BSP.Rotate),
    ("M-i", sendMessage BSP.Swap),
    ("M-n", sendMessage BSP.FocusParent),
    ("M-S-n", sendMessage BSP.MoveNode),
    ("M-C-n", sendMessage BSP.SelectNode),
    ("M-M1-j", sendMessage $ BSP.SplitShift Prev),
    ("M-M1-k", sendMessage $ BSP.SplitShift Next),
    ("M-/", sendMessage BSP.Balance),
    ("M-S-/", sendMessage BSP.Equalize),
    -- minimize
    ("M-C-m", withFocused minimizeWindow),
    ("M-C-S-m", withLastMinimized maximizeWindow)
  ]

delKeysP :: [String]
delKeysP =
  [ "M-S-c"
  ]

addKeys :: [((KeyMask, KeySym), X ())]
addKeys =
  [ ((myModMask .|. m, kc), withNthWorkspace f w)
    | (kc, w) <- zip ([xK_1 .. xK_9] ++ [xK_0]) [0 ..],
      (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]
    ++ [ ((myModMask .|. m, kc), screenWorkspace sc >>= flip whenJust (windows . f))
         | (kc, sc) <- zip ([xK_1 .. xK_9] ++ [xK_0]) [0 ..],
           (f, m) <- [(W.view, mod1Mask), (W.shift, mod1Mask .|. shiftMask)]
       ]

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal =
        (D.signal objectPath interfaceName memberName)
          { D.signalBody = [D.toVariant $ UTF8.decodeString str]
          }
  D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

main :: IO ()
main = do
  notifClient <- N.connectSession
  dbus <- D.connectSession

  D.requestName
    dbus
    (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  xmonad $
    docks . ewmh . ewmhFullscreen $
      def
        { modMask = myModMask,
          normalBorderColor = C.color7,
          focusedBorderColor = C.color8,
          borderWidth = 2,
          terminal = "xdg-terminal-exec",
          workspaces = ["home", "web", "dev", "music", "games"],
          startupHook = myStartupHook,
          manageHook = myManageHook,
          layoutHook = myLayoutHook,
          handleEventHook = myHandleEventHook,
          logHook = dynamicLogWithPP $ myPP dbus
        }
        `additionalKeysP` addKeysP notifClient
        `removeKeysP` delKeysP
        `additionalKeys` addKeys
