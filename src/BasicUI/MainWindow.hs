
module BasicUI.MainWindow where

import Graphics.Vty

import BasicUI.UI
import BasicUI.HEvent
import BasicUI.Widget

import qualified Data.Map as Map

data MainWindow w = MainWindow {
  mwID       :: SwID
, mwContent  :: w
, mwPos      :: (Int, Int)
, mwSize     :: (Int, Int)
, mwHasFocus :: Bool
, mwExit     :: Bool
, mwTheme    :: Color
, mwDragAnchor :: Maybe (Int, Int)
}

class (Widget wiw) => WidgetInWindow wiw where
  wiwTitle  :: wiw -> String

instance (WidgetInWindow wiw) => Widget (MainWindow wiw) where
  wUpdater mw = withOffset (mwPos mw) $ mempty
    <> mwFocusHandler
    <> mwExitHandler
    <> mwDragHandler
    <> mwContentEventHandler
    <> mwEventCatcher
  
  wPaint mw = shiftUI (mwPos mw) $ mempty
    <> mwFgPainter mw
    <> (shiftUI (1,1) $ wPaint (mwContent mw))
    <> mwBgPainter mw


mwResizeHandler :: WidgetInWindow wiw => HMap (MainWindow wiw)
mwResizeHandler = HMap $ \(e, mw) -> case e of
  HEvent { hEvent = EvResize x y } ->
    let
      (sx, sy) = mwSize mw
      e' = e { hEvent = EvResize (sx-2) (sy-2) }
      wiw = mwContent mw
      contentUpdater = wUpdater wiw
      (e'', wiw') = runHMap contentUpdater (e', wiw)
    in
      (e'' { hEvent = EvResize x y }, mw { mwContent=wiw' })
  otherwise -> (e, mw)

mwDragHandler :: WidgetInWindow wiw => HMap (MainWindow wiw)
mwDragHandler = HMap $ \(e, mw) ->
  case mwHasFocus mw of
    True -> case mwDragAnchor mw of
      Just (ax, ay) -> case hEvent e of
        EvMouseDown cx cy BLeft [] ->
          let
            (px,py) = mwPos mw
            mw' = mw { mwPos=(px+cx-ax,py+cy-ay) }
          in
            (evClose e "[mwDragHandler]: lifted ", mw')
        EvMouseUp cx cy _ ->
          let
            (px,py) = mwPos mw
            mw' = mw { mwPos=(px+cx-ax,py+cy-ay), mwDragAnchor=Nothing }
          in
            (evClose e "[mwDragHandler]: dropped ", mw')
        otherwise -> (e, mw)
      Nothing -> case hIsFirstPress e of
        Just True -> case hEvent e of
          EvMouseDown cx 0 BLeft [] ->
            let
              sx = fst $ mwSize mw
              onAnchorArea = cx >= 0 && cx < sx - 3
              (e', mw') = if onAnchorArea then (evClose e "[mwDragHandler]: ready ", mw { mwDragAnchor=Just (cx,0) }) else (e, mw)
            in
              (e', mw')
          otherwise -> (e, mw)
        otherwise -> (e, mw)
    False -> (e, mw)
      
mwContentEventHandler :: WidgetInWindow wiw => HMap (MainWindow wiw)
mwContentEventHandler = HMap $ \(e, mw) ->
  let
    wiw = mwContent mw
    contentUpdater = wUpdater wiw
    (e', wiw') = case e of
      HEvent { hEvent = EvResize x y } ->
        let
          (sx, sy) = mwSize mw
          wiwe = e { hEvent = EvResize (sx-2) (sy-2) }
          (wiwe', wiw') = runHMap contentUpdater (wiwe, wiw)
        in
          (wiwe' { hEvent = EvResize x y }, wiw')
      otherwise -> runHMap contentUpdater (e, wiw)
  in
    (e', mw { mwContent=wiw' })

mwExitHandler :: WidgetInWindow w => HMap (MainWindow w)
mwExitHandler = HMap $ \(e, mw) ->
  case mwHasFocus mw of
    False -> (e, mw)
    True -> case hEvent e of
      EvMouseUp cx 0 (Just BLeft) ->
        if cx == (fst $ mwSize mw) - 2
        then
          (evClose e "[mwExitHandler]: mwExit ", mw { mwExit=True, mwHasFocus=False, mwDragAnchor=Nothing })
        else
          (e, mw)
      otherwise -> (e, mw)

mwBgPainter :: WidgetInWindow w => MainWindow w -> UI
mwBgPainter mw = charFillRect (0,0) (mwSize mw) (mix white white) ' '

mwFgPainter :: WidgetInWindow w => MainWindow w -> UI
mwFgPainter mw =
  let
    title = (wiwTitle.mwContent) mw
    gray = srgbColor 200 200 200
    mwColor = if (mwHasFocus mw) then mwTheme mw else gray
    (sx, sy) = mwSize mw
    exitButtonUI = UI [("X", mix mwColor white, (sx - 2, 0))]
    winUI = classicWindows title (mwSize mw) mwColor
  in mempty
    <> exitButtonUI
    <> winUI

mwFocusHandler  :: WidgetInWindow wiw => HMap (MainWindow wiw)
mwFocusHandler = HMap $ \(e, mw) ->
  case hIsFirstPress e of
    Just True -> case hEvRes e of
      EvHandledBy _ -> (e, mw { mwHasFocus=False })
      EvUnhandled -> case hEvent e of
        EvMouseDown x y b ms ->
          let
            (sx, sy) = mwSize mw
            newFocus = x >= 0 && y >= 0 && x < sx && y < sy
            (e', mw') = case (mwHasFocus mw, newFocus) of
              (False, True) -> (evClose e "[mwFocusHandler]: acquired", mw { mwHasFocus=True })
              (True, False) -> (e, mw { mwHasFocus=False })
              otherwise -> (e, mw)
          in
            (e', mw')
        otherwise -> (e, mw)
    otherwise -> (e, mw)

mwEventCatcher :: WidgetInWindow wiw => HMap (MainWindow wiw)
mwEventCatcher = HMap $ \(e, mw) ->
  case hEvent e of
    EvMouseDown x y _ _ ->
      let
        (sx, sy) = mwSize mw
        catched = x >= 0 && y >= 0 && x < sx && y < sy
        e' = if catched then evClose e "[mwEventCatcher]: " else e
      in
        (e', mw)
    
    EvMouseUp x y _ ->
      let
        (sx, sy) = mwSize mw
        catched = x >= 0 && y >= 0 && x < sx && y < sy
        e' = if catched then evClose e "[mwEventCatcher]: " else e
      in
        (e', mw)
    otherwise -> (e, mw)


instance MoveAble (MainWindow a) where
  maPos = mwPos
  maSetPos p a = a { mwPos=p }
