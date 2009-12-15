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
-- Win-Shift+1..9       move window to workspace
-- Win+Up/Down          move window up/down
-- Win+C                close window
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
-- Win+R                open the Gnome run dialog
-- Win+Q                restart XMonad
-- Win+Shift+Q          display Gnome shutdown dialog

import XMonad
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as S
import XMonad.Actions.CycleWS
import XMonad.Actions.Plane
import XMonad.Actions.GridSelect
import XMonad.Config.Gnome
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
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
import XMonad.Layout.SimpleDecoration
import XMonad.Util.WindowProperties
import Control.Monad
import Data.Ratio
import qualified Data.Map as M

-- defaults on which we build
-- use e.g. defaultConfig or gnomeConfig
myBaseConfig = gnomeConfig

-- display
-- replace the bright red border with a more stylish colour
myBorderWidth = 2
myNormalBorderColor = "#202030"
myFocusedBorderColor = "#A0A0D0"

  -- workspaces
myWorkspaces = ["web1", "web2", "im"] ++ (miscs 5) ++ ["fullscreen"]
  where miscs = map (("misc" ++) . show) . (flip take) [1..]
isFullscreen = (== "fullscreen")

  -- layouts
basicLayout = Tall nmaster delta ratio where
nmaster = 1
delta   = 3/100
ratio   = 1/2
tallLayout = named "tall" $ avoidStruts $ basicLayout
wideLayout = named "wide" $ avoidStruts $ Mirror basicLayout
tabbedLayout = named "tabbed" $ avoidStruts $ simpleTabbed
singleLayout = named "single" $ avoidStruts $ noBorders Full
fullscreenLayout = named "fullscreen" $ noBorders Full
imLayout = avoidStruts $ reflectHoriz $ withIMs ratio rosters chatLayout where
  chatLayout      = Grid
  ratio           = 1%6
  rosters         = [skypeRoster, pidginRoster]
  pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
  skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))

myLayoutHook = showWName $ fullscreen $ im $ normal where
  normal     = tallLayout ||| wideLayout ||| singleLayout ||| tabbedLayout
  fullscreen = onWorkspace "fullscreen" fullscreenLayout
  im         = onWorkspace "im" imLayout

  -- special treatment for specific windows:
  -- put the Pidgin and Skype windows in the im workspace
myManageHook = myFloatHooks <+> imManageHooks <+> manageHook myBaseConfig
imManageHooks = composeAll [isIM --> moveToIM] where
  isIM     = foldr1 (<||>) [isPidgin, isSkype]
  isPidgin = className =? "Pidgin"
  isSkype  = className =? "Skype"
  moveToIM = doF $ S.shift "im"

myFloatHooks = composeAll [resource =? "Do" --> doIgnore,
                          resource =? "toggl" --> doIgnore,
                          className =? "com-yuuguu-client-bootstrapping-YuuguuBootStrapMain" --> doFloat,
                          className =? "XTerm" --> doFloat,
                          resource =? "tomboy" --> doFloat]

  -- Mod4 is the Super / Windows key
myModMask = mod4Mask
altMask = mod1Mask

  -- better keybindings for dvorak
myKeys conf = M.fromList $
  [ ((myModMask              , xK_t     ), spawn $ XMonad.terminal conf)
  --, ((myModMask              , xK_r     ), spawn $ (XMonad.terminal conf) ++ " -e irb") --Annoying with Redcar
  , ((myModMask              , xK_f     ), goToSelected defaultGSConfig)
  , ((myModMask              , xK_c     ), kill)
  , ((altMask                , xK_F4    ), kill)
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
  , ((myModMask .|. shiftMask, xK_q     ), spawn "gnome-session-save --kill")
  ] 
  
  ++
  -- Win+1..10 switches to workspace
  [ ((myModMask, k), windows $ S.greedyView i)
    | (i, k) <- zip myWorkspaces workspaceKeys
  ] 
  
  ++
  -- mod+shift+1..10 moves window to workspace and switches to that workspace
  [ ((myModMask .|. shiftMask, k), (windows $ S.shift i) >> (windows $ S.greedyView i))
    | (i, k) <- zip myWorkspaces workspaceKeys
  ] 

  ++

  [ ((ctrlAlt .|. mask, k), function (Lines 3) Finite direction)
    | (k, direction) <- zip [xK_Left .. xK_Down] $ enumFrom ToLeft 
    , (mask, function) <- [(0, planeMove), (shiftMask, planeShift)]]
    where 
      workspaceKeys = [xK_1 .. xK_9]
      ctrlAlt = controlMask .|. altMask
        

-- mouse bindings that mimic Gnome's
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((altMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((altMask, button2), (\w -> focus w >> (withFocused $ windows . S.sink)))
    , ((altMask, button3), (\w -> focus w >> mouseResizeWindow w)) --Right-click
    , ((altMask, button4), (const $ windows S.swapUp))
    , ((altMask, button5), (const $ windows S.swapDown))
    ]

-- put it all together
main = xmonad $ myBaseConfig
                {   modMask = myModMask
                  , workspaces = myWorkspaces
                  , layoutHook = myLayoutHook
                  , manageHook = myManageHook
                  , borderWidth = myBorderWidth
                  , normalBorderColor = myNormalBorderColor
                  , focusedBorderColor = myFocusedBorderColor
                  , keys = myKeys
                  , mouseBindings = myMouseBindings
                }

-- modified version of XMonad.Layout.IM --

  -- | Data type for LayoutModifier which converts given layout to IM-layout
  -- (with dedicated space for the roster and original layout for chat windows)
data AddRosters a = AddRosters Rational [Property] deriving (Read, Show)

instance LayoutModifier AddRosters Window where
  modifyLayout (AddRosters ratio props) = applyIMs ratio props
  modifierDescription _                = "IMs"

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

