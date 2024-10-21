{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Loop (numLoop)
import SDL
import qualified SDL.Image as Image
import qualified Store as S
import Win

setfps :: IO ()
setfps = threadDelay 16_000 -- 60fps

main :: IO ()
main = do
  initializeAll
  window <-
    createWindow
      "Hello SDL"
      defaultWindow {windowInitialSize = V2 600 480} -- 600 480
  SDL.showWindow window
  rootImage <- loadScreenshoot
  S.savePng rootImage "/home/sendai/test.png"
  surface <- Image.load "/home/sendai/test.png"
  renderer <- createRenderer window (-1) defaultRenderer
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

  texture <- createTextureFromSurface renderer surface

  numLoop @Int 1 (60 * 5) $ \n -> do
    clear renderer
    copy renderer texture (Just (Rectangle (P (V2 0 0)) (V2 100 100))) Nothing
    present renderer
    setfps
    print n

  destroyRenderer renderer
  destroyWindow window
  quit
