{-# LANGUAGE OverloadedStrings #-}
module Game where

import Graphics.Vty

import BasicUI.UI
import BasicUI.HEvent
import BasicUI.Widget
import BasicUI.MainWindow

import GameWidget.Terminal
import GameWidget.DebugPanel
import GameWidget.EventPanel


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

game = Terminal {
  tName = "HiveGame: press space to reopen closed windows, q to quit"
, tSize = (10, 5)
, tDebugPanel=debugPanel
, tEventPanel=eventPanel
, tExit=Nothing
, tSwIdList=["Event Panel", "Debug Panel"]
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
