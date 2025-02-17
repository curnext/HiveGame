

module Main where

import Graphics.Vty

import BasicUI.UI
import BasicUI.HEvent
import Game

import Control.Exception (catch, SomeException)


main :: IO ()
main = do
  (ie,vty) <- initGame
  catch (eventLoop vty (ie, game)) $ handler vty
  shutdown vty

eventLoop :: Game g => Vty -> (HEvent, g) -> IO()
eventLoop vty (e, game) = do
  let (e', game', ui) = gameUpdate (e, game)
  update vty $ renderPicture ui
  case gameExit game' of
    Just n -> return ()
    Nothing -> do
      e' <- nextHEvent e' vty
      eventLoop vty (e', game')

handler :: Vty -> SomeException -> IO ()
handler vty e = do
  shutdown vty
  putStrLn $ "Unhandled exception: " ++ show e
  fail "Terminating program due to an exception"
