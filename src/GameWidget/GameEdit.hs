module GameWidget.GameEdit where

import Graphics.Vty

import BasicUI.Widget
import BasicUI.UI
import BasicUI.HEvent
import BasicUI.MainWindow

import Hive.Hid
import Hive.Grid
import Hive.Tools

import qualified Data.Map as Map


data GameEdit = GameEdit {
  geSize        :: (Int, Int)
, geOrigin      :: (Int, Int)
, geMappers     :: ([HiveGridMapper], HiveGridMapper, [HiveGridMapper])
, geCellNumbers :: Map.Map CID Int
}

instance HiveGrid GameEdit where
  hgSize = geSize
  hgSetSize hg size = hg { geSize=size }
  hgOrigin = geOrigin
  hgSetOrigin hg origin = hg { geOrigin=origin }
  hgMappers = geMappers
  hgSetMappers hg ms = hg { geMappers=ms }

instance MoveAble GameEdit where
  maPos = geOrigin
  maSetPos origin ge = ge { geOrigin=origin }

instance Widget GameEdit where
  wUpdater ge = mempty

  wPaint ge = shiftUI (geOrigin ge) (gePaint ge)

instance WidgetInWindow GameEdit where
  wiwTitle = const "Game Edit"


gePaint :: GameEdit -> UI
gePaint ge =
  let
    numbers = geCellNumbers ge
    HiveGridMapper cxy2Hid cid2Cxy lid2Cxy nid2Cxy = hgMapper ge
    lines = cellsBorders numbers
    nodes = cellsCorners numbers
    paintLid lid =  map (\(lx, ly) -> ("+", defAttr, (lx, ly))) $ lid2Cxy lid
    linesUI = foldMap paintLid lines
    paintCid cid = ("@", defAttr, cid2Cxy cid)
    cellsUI = map paintCid $ Map.keys numbers
    paintNid nid = [("#", defAttr, nid2Cxy nid)]
    nodesUI = foldMap paintNid nodes
  in
    (UI linesUI) <> (UI cellsUI) <> (UI nodesUI)