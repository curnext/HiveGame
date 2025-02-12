{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module Widget where

import Graphics.Vty

type UI = [(String, Attr, (Int, Int))]


class Widget w where
  wName     :: w -> String
  wSize     :: w -> (Int, Int)
  wPos      :: w -> (Int, Int)

  wSetPos   :: (Int, Int) -> w -> w

  _wUpdate      :: Event -> w -> w
  _wUpdate (EvKey key ms) = wOnKey key ms
  _wUpdate (EvMouseDown x y b ms) = wOnMouseDown (x, y) b ms
  _wUpdate (EvMouseUp x y mb) = wOnMouseUp (x, y) mb
  _wUpdate (EvResize x y) = wOnResize (x, y)
  _wUpdate _ = id
  
  wOnKey        :: Key -> [Modifier] -> w -> w
  wOnMouseDown  :: (Int, Int) -> Button -> [Modifier] -> w -> w
  wOnMouseUp    :: (Int, Int) -> Maybe Button -> w -> w
  wOnResize     :: (Int, Int) -> w -> w
  wOnKey _ _ = id
  wOnMouseDown _ _ _ = id
  wOnMouseUp _ _ = id
  wOnResize _ = id

  wPaint        :: w -> UI

  wChildren     :: w -> [SomeWidget]
  wChildren = const []

  _wRender      :: w -> UI
  _wRender w =
    let
      _wRenderChild (SomeWidget c) =
        let
          rawImages = wPaint c
          (px,py) = wPos c
          images = map (\(s,a,(ix,iy)) -> (s,a,(ix+px,iy+py))) rawImages
        in
          images
      children = wChildren w
      childrenLayers = concatMap _wRenderChild children
      images = childrenLayers ++ wPaint w
    in
      images

data SomeWidget = forall w. Widget w => SomeWidget w

data Terminal = Terminal {
  _tName        :: String
, _tSize        :: (Int, Int)
, _tMainWindows :: [SomeWidget]
}

instance Widget Terminal where
  wName = _tName
  wSize = _tSize
  wPos = const (0, 0)
  -- I need somehow to log events to DebugPanel like this resize event
  wOnResize size t = t { _tSize=size }
  wOnMouseDown pos BLeft _ t =
    let
      mws = _tMainWindows t
      mws' = map (\(SomeWidget mw) -> SomeWidget $ wSetPos pos mw) mws
    in
      t { _tMainWindows=mws' }
  wOnMouseDown _ _ _ t = t

  wPaint t =
    let
      (x, y) = wSize t
      top = replicate x '#'
      middle = '#':(replicate (x-2) ' ')++['#']
      bottom = top
      lns = top:(replicate (y-2) middle)++[bottom]
      images = map (\(ln, i) -> (ln, defAttr, (0,i))) $ zip lns [0..]
      title = (wName t, defAttr, (0,0))
    in
      title:images
    
  wChildren t = _tMainWindows t

  wSetPos _ = id

data DebugPanel = DebugPanel {
  _dpName     :: String
, _dpSize     :: (Int, Int)
, _dpPos      :: (Int, Int)
, logLines    :: [(String, Attr)]
}

instance Widget DebugPanel where
  wName = _dpName
  wSize = _dpSize
  wPos = _dpPos

  wSetPos pos w = w { _dpPos=pos }

  wPaint dp =
    let
      (x, y) = wSize dp
      top = replicate x '='
      middle = '|':(replicate (x-2) ' ')++['|']
      bottom = replicate x '*'
      lns = top:(replicate (y-2) middle)++[bottom]
      images = map (\(ln, i) -> (ln, defAttr, (0,i))) $ zip lns [0..]
      title = (wName dp, defAttr, (0,0))
      logs = map (\((s, a), i) -> (s, a, (1, i+1))) $ zip (logLines dp) [0..]
    in
      title:logs++images
