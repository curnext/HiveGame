{-# LANGUAGE NamedFieldPuns #-}

module GameWidget.Terminal where

import Graphics.Vty

import BasicUI.UI
import BasicUI.HEvent
import BasicUI.Widget
import BasicUI.MainWindow

import GameWidget.DebugPanel
import GameWidget.EventPanel

import qualified Data.Map as Map
import Data.List (nub)

data Terminal = Terminal {
  tName         :: String
, tSize         :: (Int, Int)
, tDebugPanel   :: MainWindow DebugPanel
, tEventPanel   :: MainWindow EventPanel
, tSwIdList     :: [SwID]
, tExit         :: Maybe Int
}

instance Widget Terminal where
  wUpdater t = mempty
    <> tExitHandler
    <> tResizeHandler
    <> tOnPressSpaceReveilHiddenWindows
    <> wcUpdater t
    <> tHandleFocusEvent
    <> HMap (\(e, t) -> (e, tLogStrLn ("__[ " ++ (show (_hID e)) ++ " ]____________________") t))
    
  wPaint t = mempty
    <> classicWindows (tName t) (tSize t) blue
    <> wcPaint t
    <> charFillRect (0,0) (tSize t) (mix white white) ' '
      
instance WidgetContainer Terminal where
  wcIdList = tSwIdList

  wcUpdaters t =
    let
      ep = tEventPanel t
      dp = tDebugPanel t
      mws = [
        (mwExit ep, mwID ep, wcRegister tEventPanel (\t c -> t { tEventPanel=c }))
        , (mwExit dp, mwID dp, wcRegister tDebugPanel (\t c -> t { tDebugPanel=c }))
        ]
    in
      Map.fromList [(swID, updater)| (exit, swID, updater) <- mws, not exit]

  wcPainters t = 
    let
      ep = tEventPanel t
      dp = tDebugPanel t
      mws = [
        (mwExit ep, mwID ep, \t -> wPaint (tEventPanel t))
        , (mwExit dp, mwID dp, \t -> wPaint (tDebugPanel t))
        ]
    in
      Map.fromList [(swID, painter)| (exit, swID, painter) <- mws, not exit]


tExitHandler    :: HMap Terminal
tExitHandler = HMap $ \(e, t) -> case e of
  HEvent { hEvent = EvKey (KChar 'q') [] } -> (evClose e "[tExitHandler]: ", t{tExit=Just 0})
  otherwise -> (e, t)

tResizeHandler  :: HMap Terminal
tResizeHandler = HMap $ \(e, t) -> case e of
  HEvent { hEvent = EvResize x y } -> (evClose e "[tResizeHandler]: ", t { tSize=(x,y) })
  otherwise -> (e, t)

tHandleFocusEvent :: HMap Terminal
tHandleFocusEvent = HMap $ \(e, t) ->
    let
      ep = tEventPanel t
      dp = tDebugPanel t
      mws = map snd $ filter fst [ (mwHasFocus ep, mwID ep) , (mwHasFocus dp, mwID dp) ]
    in
      case mws of
        [] -> (e, t)
        swID:_ -> (e, t { tSwIdList=nub (swID:(tSwIdList t)) })

tOnPressSpaceReveilHiddenWindows :: HMap Terminal
tOnPressSpaceReveilHiddenWindows = HMap $ \(e, t) ->
  case hEvent e of
    EvKey (KChar ' ') _ ->
      let
        dpmw = tDebugPanel t
        epmw = tEventPanel t
        t' = t {tDebugPanel=dpmw{mwExit=False}, tEventPanel=epmw{mwExit=False}}
      in
        (evClose e "[tOnPressSpaceReveilHiddenWindows]: ", t')
    otherwise -> (e, t)


tLogStrLn :: String -> Terminal -> Terminal
tLogStrLn l t =
  let
    dpmw = tDebugPanel t
    dp = mwContent dpmw
    lns = dpLogLines dp
    dp' = dp { dpLogLines=l:lns }
    dpmw' = dpmw { mwContent=dp' }
  in
    t { tDebugPanel=dpmw'}

tLogStrLns :: [String] -> Terminal -> Terminal
tLogStrLns [] t = t
tLogStrLns (l:ls) t = tLogStrLns ls (tLogStrLn l t)
