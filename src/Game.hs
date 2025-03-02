{-# LANGUAGE OverloadedStrings #-}
module Game where

import Graphics.Vty

import BasicUI.UI
import BasicUI.HEvent
import BasicUI.Widget
import BasicUI.MainWindow

import Hive.Hid
import Hive.Grid

import GameWidget.Terminal
import GameWidget.DebugPanel
import GameWidget.EventPanel
import GameWidget.GameEdit

import qualified Data.Map as Map


debugPanel = MainWindow {
  mwContent = DebugPanel {
    dpName = "Debug Panel: Handled Events"
  , dpSize = (0, 0)
  , dpLogLines = []
  }
, mwID = "Debug Panel"
, mwPos = (5, 4)
, mwSize = (90, 18)
, mwHasFocus = False
, mwExit = False
, mwTheme = red
, mwDragAnchor = Nothing
}

eventPanel = MainWindow {
  mwContent =   EventPanel {
    epName = "Event Panel: Unhandled Events"
  , epSize = (0, 0)
  , epLogLines = []
  }
, mwID = "Event Panel"
, mwPos = (80,12)
, mwSize = (90,20)
, mwHasFocus = False
, mwExit = False
, mwTheme = green
, mwDragAnchor = Nothing
}

gameEdit = MainWindow {
  mwContent = GameEdit {
    geSize = (0,0)
  , geOrigin = (14, 5)
  , geMappers = ([s2MapperA, s1MapperA], s3MapperA, [s4MapperA])
  , geCellNumbers = Map.fromList
      [(Cell hx hy, hx)
      | hx <- [-1..1]
      , hy <- [-1..1]
      ]
  }
, mwID = "Game Edit"
, mwPos = (40,6)
, mwSize = (30,12)
, mwHasFocus = False
, mwExit = False
, mwTheme = yellow
, mwDragAnchor = Nothing
}

game = Terminal {
  tName = "HiveGame: press space to reopen closed windows, q to quit"
, tSize = (10, 5)
, tDebugPanel=debugPanel
, tEventPanel=eventPanel
, tGameEdit=gameEdit
, tExit=Nothing
, tSwIdList=[ mwID eventPanel
            , mwID gameEdit
            , mwID debugPanel]
}


class Game g where
  gameUpdate :: (HEvent, g) -> (HEvent, g, UI)
  gameExit :: g -> Maybe Int

instance Game Terminal where
  gameUpdate (e, t) =
    let
      (e', t') = runHMap (wUpdater t) (e, t)
      ui = wPaint t'
    in (e', t', ui)
  gameExit = tExit
