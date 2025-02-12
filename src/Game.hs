{-# LANGUAGE OverloadedStrings #-}
module Game where

import Graphics.Vty
import Widget

debugPanel = DebugPanel {
_dpName = "DebugPanel"
, _dpSize = (30, 14)
, _dpPos = (5, 2)
, logLines = [("HiveGame Log:", defAttr), ("...", defAttr)]
}

terminal = Terminal {
  _tName = "HiveGame"
, _tSize = (0, 0)
, _tMainWindows = [SomeWidget debugPanel]
}
