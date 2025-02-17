{-# LANGUAGE NamedFieldPuns #-}

module BasicUI.HEvent where

import Graphics.Vty
import Graphics.Vty.CrossPlatform (mkVty)

import BasicUI.UI


data EvResult = EvUnhandled | EvHandledBy String
  deriving ( Show )

data HEvent = HEvent {
  hEvent  :: Event
, hEvRes  :: EvResult
, hIsFirstPress   :: Maybe Bool
, _hLog   :: [String] -- should have better way for logging
, _hID    :: Int
}

data HMap a = HMap {
  runHMap :: (HEvent, a) -> (HEvent, a)
}

instance Semigroup (HMap a) where
  HMap m1 <> HMap m2 = HMap (m2.m1)

instance Monoid (HMap a) where
  mempty = HMap id

instance ShiftAble Event where
  (EvMouseDown x y b ms) <-<< (ix, iy) = EvMouseDown (x-ix) (y-iy) b ms
  (EvMouseUp x y mb) <-<< (ix, iy) = EvMouseUp (x-ix) (y-iy) mb
  e <-<< _ = e

instance ShiftAble HEvent where
  e@HEvent{ hEvent } <-<< ixy = e { hEvent = hEvent <-<< ixy }

instance Show HEvent where
  show hev =
    let
      hevs = show (hEvent hev)
      ifps = case hIsFirstPress hev of
        Nothing -> " "
        Just True -> "!"
        Just False -> "-"
      res = case hEvRes hev of
        EvHandledBy _ -> " "
        EvUnhandled -> "*"
      hids = "[" ++ (show (_hID hev)) ++ "]"
    in
      hids ++ ifps ++ res ++ hevs


initGame :: IO (HEvent, Vty)
initGame = do
  vty <- mkVty defaultConfig
  setMode (outputIface vty) Mouse True
  (ix,iy) <- displayBounds (outputIface vty)
  let ie = HEvent { hEvent=EvResize ix iy, hEvRes=EvUnhandled, hIsFirstPress=Nothing, _hLog=mempty, _hID=0 }
  return (ie, vty)

_log :: String -> HEvent -> HEvent
_log s e@HEvent{_hLog} = e {_hLog=s:_hLog}

evClose :: HEvent -> String -> HEvent
evClose e str = e { hEvRes=EvHandledBy (str ++ show e) }

nextHEvent :: HEvent -> Vty -> IO HEvent
nextHEvent hev vty = do
  ev <- nextEvent vty
  let ifp = (hIsFirstPress hev)
  let nid = _hID hev + 1
  case ev of
    EvMouseDown _ _ _ _ -> case ifp of
      Just _  -> return HEvent { hEvent=ev, hEvRes=EvUnhandled, hIsFirstPress=Just False, _hLog=mempty, _hID=nid }
      Nothing -> return HEvent { hEvent=ev, hEvRes=EvUnhandled, hIsFirstPress=Just True, _hLog=mempty, _hID=nid }
    EvMouseUp _ _ _ -> return HEvent { hEvent=ev, hEvRes=EvUnhandled, hIsFirstPress=Nothing, _hLog=mempty, _hID=nid }
    otherwise -> return HEvent { hEvent=ev, hEvRes=EvUnhandled, hIsFirstPress=ifp, _hLog=mempty, _hID=nid }

withOffset :: MoveAble w => (Int, Int) -> HMap w -> HMap w
withOffset ixy subHandler = (HMap $ \(e, w) -> (e <-<< ixy, w))
  <> subHandler
  <> (HMap $ \(e, w) -> (e >>-> ixy, w))
