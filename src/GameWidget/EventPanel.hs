module GameWidget.EventPanel where

import Graphics.Vty

import BasicUI.Widget
import BasicUI.UI
import BasicUI.HEvent
import BasicUI.MainWindow


data EventPanel = EventPanel {
  epName     :: String
, epSize     :: (Int, Int)
, epLogLines   :: [String]
}

instance Widget EventPanel where
  wUpdater w = mempty
    <> epResizeHandler
    <> epLogAllEvents
    
  wPaint w = mempty
    <> epPainter w

instance WidgetInWindow EventPanel where
  wiwTitle = epName


epResizeHandler  :: HMap EventPanel
epResizeHandler = HMap $ \(e, ep) -> case e of
  HEvent { hEvent = EvResize x y } -> (e, ep { epSize=(x,y), epName = (show e) })
  otherwise -> (e, ep)

epLogAllEvents :: HMap EventPanel
epLogAllEvents = HMap $ \(e, ep) -> case hEvRes e of
  EvUnhandled -> (e, ep { epLogLines=(show e):(epLogLines ep) })
  otherwise -> (e, ep)

epPainter :: EventPanel -> UI
epPainter ep = UI $ map (\(s, i) -> (s, (mix white red), (0, i))) $ zip (epLogLines ep) [0..(snd $ epSize ep)]


