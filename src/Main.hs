

module Main where

import Control.Exception (catch, SomeException)
import Graphics.Vty
import Graphics.Vty.CrossPlatform (mkVty)
import Widget
import Game


main :: IO ()
main = do
  vty <- mkVty defaultConfig
  setMode (outputIface vty) Mouse True
  initSize <- displayBounds (outputIface vty)
  let rootWidget = terminal { _tSize = initSize }

  catch (eventLoop vty rootWidget) $ handler vty
  shutdown vty

eventLoop :: Widget w => Vty -> w -> IO()
eventLoop vty w = do
  let pic = renderPicture $ _wRender w
  update vty pic
  ev <- nextEvent vty
  let w' = _wUpdate ev w
  case ev of
    EvKey (KChar 'q') []  -> return ()
    _                     -> eventLoop vty w'
  
handler :: Vty -> SomeException -> IO ()
handler vty e = do
  shutdown vty
  putStrLn $ "Unhandled exception: " ++ show e
  fail "Terminating program due to an exception"

renderPicture :: [(String, Attr, (Int, Int))] -> Picture
renderPicture xs =
  let
    toImage :: (String, Attr, (Int, Int)) -> Image
    toImage (s,a,(x,y)) = translate x y $ string a s
    images = map toImage xs
  in
    Picture NoCursor images ClearBackground