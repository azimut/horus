{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Loop (numLoop)
import qualified Foreign as F
import Foreign.C.Types as F
import SDL
import qualified SDL.Raw.Types as ST
import qualified SDL.Raw.Video as V
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
      defaultWindow {windowInitialSize = V2 600 480}
  winSurface <- getWindowSurface window
  rootImage <- loadScreenshoot
  S.savePng rootImage "/home/sendai/test.png"
  -- surface <- surfaceFromImage rootImage
  -- renderer <- V.createRenderer _ (-1) 0
  -- texture <- V.createTextureFromSurface renderer surface

  surfaceFillRect winSurface Nothing (V4 0 0 127 0)

  numLoop @Int 1 (60 * 5) $ \n -> do
    updateWindowSurface window
    setfps
    print n

  freeSurface winSurface
  destroyWindow window
  quit

surfaceFromImage :: RawImage a -> IO (F.Ptr ST.Surface)
surfaceFromImage RawImage {..} =
  V.createRGBSurfaceFrom
    (F.castPtr rawImagePtr)
    (int rawImageWidth)
    (int rawImageHeight)
    (int rawImageDepth)
    0
    0
    0
    0
    0
  where
    int i = fromIntegral i :: F.CInt

-- main :: IO ()
-- main = do
--   img <- W.loadScreenshoot
--   S.save img "/home/sendai/test.png"
--   putStrLn "Done!"
