module GameWidget.DebugPanel where

import Graphics.Vty

import BasicUI.Widget
import BasicUI.UI
import BasicUI.HEvent
import BasicUI.MainWindow


data DebugPanel = DebugPanel {
  dpName      :: String
, dpSize      :: (Int, Int)
, dpLogLines  :: [String]
}

instance Widget DebugPanel where
  wUpdater w = mempty
    <> dpResizeHandler
    <> dpLogAllEvents
    
  wPaint w = mempty
    <> dpPainter w

instance WidgetInWindow DebugPanel where
  wiwTitle = dpName


dpResizeHandler  :: HMap DebugPanel
dpResizeHandler = HMap $ \(e, dp) -> case e of
  HEvent { hEvent = EvResize x y } -> (e, dp { dpSize=(x,y), dpName = (show e) })
  otherwise -> (e, dp)

dpLogAllEvents :: HMap DebugPanel
dpLogAllEvents = HMap $ \(e, dp) -> case hEvRes e of
  EvHandledBy w  -> (e, dp { dpLogLines=w:(dpLogLines dp) })
  otherwise -> (e, dp)

dpPainter :: DebugPanel -> UI
dpPainter dp = UI $ map (\(s, i) -> (s, (mix white red), (0, i))) $ zip (dpLogLines dp) [0..(snd $ dpSize dp)]


