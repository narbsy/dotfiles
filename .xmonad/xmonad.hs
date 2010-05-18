{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}

-- XMonad configuration file by Thomas ten Cate <ttencate@gmail.com>
--  Modified by Chris Narburgh <chris.narburgh@gmail.com>
-- 
-- Works on xmonad-0.8, NOT on 0.7 or below; and of course
-- xmonad-contrib needs to be installed as well
--
-- Pidgin and Skype windows are automatically placed onto the IM workspace.
-- Their contact lists each get a column on the right side of the screen,
-- and all their other windows (chat windows, etc.) go into a grid layout
-- in the remaining space.
-- (This uses a copied and modified version of XMonad.Layout.IM.)
--
-- Navigation:
-- Win+1..9             switch to workspace
-- Ctrl-Alt-Dir         switch to workspace in direction dir, as if they were arrayed on a plane
-- Alt+Tab              focus next window
-- Alt+Shift+Tab        focus previous window
--
-- Window management:
-- Win-Shift+1..9       move window to workspace, and follow
-- Win+Up/Down          move window up/down
-- Win+C                close window
-- Alt-F4               "   "   "
-- Alt+ScrollUp/Down    move focused window up/down
-- Win+M                move window to master area
-- Win+N                refresh the current window
-- Alt+Left Mouse       move floating window
-- Alt+Middle Mouse     resize floating window
-- Alt+Right Mouse      unfloat floating window
-- Win+S                unfloat floating window
--
-- Layout management:
-- Win+Left/Right       shrink/expand master area
-- Win+W/V              move more/less windows into master area
-- Win+Space            cycle layouts
--
-- Other:
-- Win+T                start a terminal
-- Win+Q                restart XMonad
-- Win+Shift+Q          display Gnome shutdown dialog

import XMonad
import qualified XMonad.StackSet as S

import XMonad.Actions.CycleWS     -- Use arrow keys to navigate betwixt workspaces
import XMonad.Actions.Plane       -- To get a 3x3 box of desktops
import XMonad.Actions.GridSelect  -- Snazzy way to select from a lot of windows. They need descriptive titles though
import XMonad.Actions.CopyWindow  -- To have a window permanently follow the desktop, or emulate this behavior.

import XMonad.Layout.Combo
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Tabbed
import XMonad.Layout.ShowWName
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.BorderResize
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
-- import XMonad.Layout.PositionStoreFloat

import XMonad.Util.WindowProperties
import XMonad.Util.EZConfig
import XMonad.Config.Gnome

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers -- isInProperty
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName

import Control.Monad
import Control.Concurrent
import Data.Ratio
import System.Exit
import System.Posix
import qualified Data.Map as M

-- defaults on which we build
-- use e.g. defaultConfig or gnomeConfig
myBaseConfig = gnomeConfig 

-- window borders
myBorderWidth = 2 -- pixels
-- black for inactive so it doesn't distract
myNormalBorderColor = "#000000"
-- white for active so it stands out
myFocusedBorderColor = "#FFFFFF"

-- workspaces
myWorkspaces = ["web1", "web2", "im"] ++ (miscs 5) ++ ["fullscreen"]
  where miscs = map (("misc" ++) . show) . (flip take) [1..]

-- layouts
basicLayout = Tall nmaster delta ratio where
  nmaster = 1       -- initial number of master windows
  delta   = 3/100   -- % change each step of window size when we resize
  ratio   = 1/2     -- The initial ratio to split windos
namedLayout name = named name . avoidStruts
tallLayout = namedLayout "tall" basicLayout
--tallLayout = named "tall" $ avoidStruts $ basicLayout
--wideLayout = named "wide" $ avoidStruts $ Mirror basicLayout
wideLayout = namedLayout "wide" $ Mirror basicLayout
-- Tabbed layout is single-layout with an extra bar on top of the windows w/ their title
--tabbedLayout = named "tabbed" $ avoidStruts $ simpleTabbed
tabbedLayout = namedLayout "tabbed" simpleTabbed
-- Single layout is full-screen that keeps the gnome-bar visible
--singleLayout = named "single" $ avoidStruts $ noBorders Full
singleLayout = namedLayout "single" $ noBorders Full
-- floatingLayout = namedLayout "floating" $ noFrillsDeco shrinkText defaultTheme $ maximize $ borderResize $ positionStoreFloat
-- Fullscreen is like single, except that it hides the gnome-bar
fullscreenLayout = named "fullscreen" $ noBorders Full

imLayout = avoidStruts $ reflectHoriz $ withIMs ratio rosters chatLayout where
  chatLayout      = Grid
  ratio           = 1%6
  rosters         = [skypeRoster, pidginRoster]
  pidginRoster    = (ClassName "Pidgin") `And` (Role "buddy_list")
  skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` 
                      (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))

myLayoutHook = showWName $ fullscreen $ im $ normal where
  normal     = tallLayout ||| wideLayout ||| singleLayout ||| tabbedLayout
  fullscreen = onWorkspace "fullscreen" fullscreenLayout
  im         = onWorkspace "im" imLayout

  -- special treatment for specific windows:
  -- put the Pidgin and Skype windows in the im workspace
myManageHook = composeAll [ myFloatHooks
                          , imManageHooks
                          , manageDocks
                          , manageHook myBaseConfig
                          ]

imManageHooks = composeAll [isIM --> moveToIM] where
  moveToIM = doF $ S.shift "im"

myLogHook = fadeOutLogHook . fadeIf q $ 0.85
  where q = isUnfocused <&&> ((liftM not) isIM)

-- used for both log hook and manage hook
isIM     = foldr1 (<||>) [isPidgin, isSkype]
isPidgin = className =? "Pidgin"
isSkype  = className =? "Skype"

myFloatHooks = composeAll [ resource  =? "Do"  --> doIgnore
                          , resource  =? "toggl"  --> doIgnore
                          , className =? "com-yuuguu-client-bootstrapping-YuuguuBootStrapMain" --> doFloat
                          , resource  =? "tomboy" --> doCenterFloat
                          , isSplash  -->  doCenterFloat
                          , isDialog  --> doCenterFloat
                          --, isSkypeWeirdness --> doFloat
                          ]

-- Found on reddit.com/r/xmonad for floating all splashy windows
isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
-- Hmmm, I wonder
--isSkypeWeirdness = (liftM not) . hasAnyProperty ["WM_WINDOW_ROLE"]
isPreferencesWindow = isInProperty "WM_WINDOW_ROLE" "Preferences"

  -- Mod4 is the Super / Windows key
myModMask = mod4Mask
altMask = mod1Mask

  -- better keybindings for dvorak
myKeys conf = M.fromList $
  [ ((myModMask              , xK_t     ), spawn $ XMonad.terminal conf)
  --, ((myModMask              , xK_r     ), spawn $ (XMonad.terminal conf) ++ " -e irb") --Annoying with Redcar
  , ((myModMask              , xK_f     ), goToSelected defaultGSConfig)
  , ((myModMask              , xK_a     ), windows copyToAll) -- Make focused window always visible
  , ((myModMask .|. shiftMask, xK_a     ), killAllOtherCopies) -- Toggle window state back to only visible on current workspace
  -- Kill me some windows
  , ((myModMask              , xK_c     ), kill)
  , ((altMask                , xK_F4    ), kill)
  -- Change layout
  , ((myModMask              , xK_space ), sendMessage NextLayout)
  , ((myModMask              , xK_n     ), refresh)
  , ((myModMask              , xK_m     ), windows S.swapMaster)
  , ((altMask                , xK_Tab   ), windows S.focusDown)
  , ((altMask .|. shiftMask  , xK_Tab   ), windows S.focusUp)
  , ((myModMask              , xK_Down  ), windows S.swapDown)
  , ((myModMask              , xK_Up    ), windows S.swapUp)
  , ((myModMask              , xK_Left  ), sendMessage Shrink)
  , ((myModMask              , xK_Right ), sendMessage Expand)
  , ((myModMask              , xK_s     ), withFocused $ windows . S.sink)
  , ((myModMask              , xK_w     ), sendMessage (IncMasterN 1))
  , ((myModMask              , xK_v     ), sendMessage (IncMasterN (-1)))
  , ((myModMask              , xK_q     ), broadcastMessage ReleaseResources >> restart "xmonad" True)
  , ((myModMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
  -- Maximizing
  , ((myModMask              , xK_z     ), withFocused (sendMessage . maximizeRestore))

  -- Minimizing
  , ((myModMask              , xK_x     ), withFocused (\f -> sendMessage (MinimizeWin f)))
  ] 
  
  -- Win+1..10 switches to workspace
  ++ [ ((myModMask, k), windows $ S.greedyView i) | (i, k) <- zippedWorkspaceKeys ] 
  
  -- mod+shift+1..10 moves window to workspace and switches to that workspace
  ++ [ ((myModMask .|. shiftMask, k), moveAndSwitch i) | (i, k) <- zippedWorkspaceKeys ] 

  -- Ctrl-Alt-Dir moves the way you think it would on a plane, no wraparound.
  --  Also maps "move window and follow" with a shift modifier.
  ++
  [ ((ctrlAlt .|. mask, k), function (Lines 3) Finite direction)
    | (k, direction) <- zip [xK_Left .. xK_Down] $ enumFrom ToLeft 
    , (mask, function) <- [(0, planeMove), (shiftMask, planeShift)]]
    where 
      workspaceKeys = [xK_1 .. xK_9]
      zippedWorkspaceKeys = zip myWorkspaces workspaceKeys
      moveAndSwitch i = (windows $ S.shift i) >> (windows $ S.greedyView i)
      ctrlAlt = controlMask .|. altMask
        

-- mouse bindings that mimic Gnome's, with the bonus of alt-right-click resize
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((altMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((altMask, button2), (\w -> focus w >> (withFocused $ windows . S.sink)))
    , ((altMask, button3), (\w -> focus w >> mouseResizeWindow w)) --Right-click
    , ((altMask, button4), (const $ windows S.swapUp))
    , ((altMask, button5), (const $ windows S.swapDown))
    ]

-- modified version of XMonad.Layout.IM --

-- | Data type for LayoutModifier which converts given layout to IM-layout
-- (with dedicated space for the roster and original layout for chat windows)
data AddRosters a = AddRosters Rational [Property] deriving (Read, Show)

instance LayoutModifier AddRosters Window where
  modifyLayout (AddRosters ratio props) = applyIMs ratio props
  modifierDescription _                 = "IMs"

-- | Modifier which converts given layout to IMs-layout (with dedicated
-- space for rosters and original layout for chat windows)
withIMs :: LayoutClass l a => Rational -> [Property] -> l a -> ModifiedLayout AddRosters l a
withIMs ratio props = ModifiedLayout $ AddRosters ratio props

-- | IM layout modifier applied to the Grid layout
gridIMs :: Rational -> [Property] -> ModifiedLayout AddRosters Grid a
gridIMs ratio props = withIMs ratio props Grid

hasAnyProperty :: [Property] -> Window -> X Bool
hasAnyProperty [] _ = return False
hasAnyProperty (p:ps) w = do
    b <- hasProperty p w
    if b then return True else hasAnyProperty ps w

-- | Internal function for placing the rosters specified by
-- the properties and running original layout for all chat windows
applyIMs :: (LayoutClass l Window) => Rational
                                   -> [Property]
                                   -> S.Workspace WorkspaceId (l Window) Window
                                   -> Rectangle
                                   -> X ([(Window, Rectangle)], Maybe (l Window))
applyIMs ratio props wksp rect = do
    let stack = S.stack wksp
    let ws = S.integrate' $ stack
    rosters <- filterM (hasAnyProperty props) ws
    let n = fromIntegral $ length rosters
    let (rostersRect, chatsRect) = splitHorizontallyBy (n * ratio) rect
    let rosterRects = splitHorizontally n rostersRect
    let filteredStack = stack >>= S.filter (`notElem` rosters)
    wrs <- runLayout (wksp {S.stack = filteredStack}) chatsRect
    return ((zip rosters rosterRects) ++ fst wrs, snd wrs)

-------------------------------------------------------------------------------
-- put it all together
main = do
        -- For fancy effects
        -- NOTE: THIS IS RIDICULOUSLY SLOW FOR SOME REASON on Gentoo
        -- spawn "xcompmgr -C"
        -- gnome-do dies for some reason when we restart if we are not it's parent. So, spawn it anyways.
        spawn "gnome-do --debug > ~/gnome-do-stdout.log 2>~/gnome-do-stderr.log"
        xmonad $ ewmh myBaseConfig {  
            modMask = myModMask
          , workspaces = myWorkspaces 
          , layoutHook = myLayoutHook
          , manageHook = myManageHook
          , logHook    = myLogHook
          , borderWidth = myBorderWidth
          , normalBorderColor = myNormalBorderColor
          , focusedBorderColor = myFocusedBorderColor
          , keys = myKeys
          , mouseBindings = myMouseBindings
          -- For java/swing compatibility
          , startupHook = setWMName "LG3D"
        }

