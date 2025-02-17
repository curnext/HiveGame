

{-# LANGUAGE NamedFieldPuns #-}

module BasicUI.Widget where

import Graphics.Vty

import BasicUI.UI
import BasicUI.HEvent

import qualified Data.Map as Map


class Widget w where
  wUpdater  :: w -> HMap w
  wPaint  :: w -> UI

type SwID = String

class WidgetContainer wc where
  wcIdList   :: wc -> [SwID]
  wcPainters :: wc -> Map.Map SwID (wc -> UI)
  wcUpdaters :: wc -> Map.Map SwID (HMap wc)

  wcPaint :: wc -> UI
  wcPaint wc = orderedMapFold (wcIdList wc) (wcPainters wc) wc
  wcUpdater :: wc -> HMap wc
  wcUpdater wc = orderedMapFold (wcIdList wc) (wcUpdaters wc)
  
  wcRegister :: Widget c => (wc -> c) -> (wc -> c -> wc) -> HMap wc
  wcRegister getChild setChild = HMap $ \(e, wc) ->
    let
      child = getChild wc
      childUpdator = wUpdater child
      (e', newChild) = runHMap childUpdator (e, child)
    in (e', setChild wc newChild)

orderedMapFold :: (Monoid m, Ord key) => [key] -> Map.Map key m -> m
orderedMapFold ks mp = foldMap (\k -> Map.findWithDefault mempty k mp) ks
  