{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Foreign.C (CInt)
import SDL
import UI.Events
import UI.Shoot
import UI.State
import Win

setfps :: IO ()
setfps = threadDelay 16_000 -- 60fps

main :: IO ()
main = do
  -- Take screenshot before drawing SDL window
  rootImage <- loadScreenshoot

  initializeAll

  window <-
    createWindow
      "Hello SDL"
      defaultWindow
        { windowInitialSize = V2 (cint (rawImageWidth rootImage)) (cint (rawImageHeight rootImage)), -- 640 360 / 600 480
          windowMode = Fullscreen
        }

  renderer <- createRenderer window (-1) defaultRenderer
  SDL.rendererDrawColor renderer $= V4 0 0 0 0

  surface <- surfaceFromPointer (rawImagePtr rootImage) (rawImageWidth rootImage) (rawImageHeight rootImage)
  texture <- createTextureFromSurface renderer surface

  SDL.showWindow window

  let loop state = do
        events <- map SDL.eventPayload <$> SDL.pollEvents
        let shouldQuit = SDL.QuitEvent `elem` events
            newState = updateEvents state events
        clear renderer
        draw renderer texture newState
        present renderer
        setfps
        when (stateScreenshootIt newState) $ do
          takeScreenshoot "/home/sendai/shoot.bmp" surface state
        unless (shouldQuit || stateQuit state || stateScreenshootIt newState) $
          loop newState

  textureInfo <- queryTexture texture
  loop
    emptyState
      { stateTextureHeight = int (textureHeight textureInfo),
        stateTextureWidth = int (textureWidth textureInfo),
        stateZoomWidth = textureWidth textureInfo,
        stateZoomHeight = textureHeight textureInfo
      }

  freeSurface surface
  destroyRenderer renderer
  destroyWindow window
  quit
  where
    int i = fromIntegral i :: Int
    cint i = fromIntegral i :: CInt
