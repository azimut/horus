{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Foreign.C (CInt)
import SDL
import UI.Draw (draw)
import UI.Events (updateEvents)
import UI.Shoot (surfaceFromPointer, takeScreenshoot)
import UI.State (State (..), emptyState)
import Win (RawImage (..), loadScreenshoot)

saveFilename :: FilePath
saveFilename = "/home/sendai/shoot.png"

setfps :: IO ()
setfps = threadDelay 16_000 -- 60fps

main :: IO ()
main = do
  -- Take screenshot before drawing SDL window
  rootImage <- loadScreenshoot

  initialize [InitVideo]

  window <-
    createWindow
      "Hello SDL"
      defaultWindow
        { windowInitialSize = V2 (cint (rawImageWidth rootImage)) (cint (rawImageHeight rootImage)), -- 640 360 / 600 480
          windowMode = Fullscreen
        }
  SDL.showWindow window
  renderer <- createRenderer window (-1) defaultRenderer
  surface <- surfaceFromPointer (rawImagePtr rootImage) (rawImageWidth rootImage) (rawImageHeight rootImage)
  texture <- createTextureFromSurface renderer surface
  textureInfo <- queryTexture texture
  let initialState =
        emptyState
          { stateTextureHeight = int (textureHeight textureInfo),
            stateTextureWidth = int (textureWidth textureInfo),
            stateZoomWidth = textureWidth textureInfo,
            stateZoomHeight = textureHeight textureInfo
          }
  draw renderer texture initialState
  present renderer

  SDL.rendererDrawColor renderer $= V4 0 0 0 0

  let loop state = do
        events <- map SDL.eventPayload <$> SDL.pollEvents
        let shouldQuit = SDL.QuitEvent `elem` events
            newState = updateEvents state events
        clear renderer
        draw renderer texture newState
        present renderer
        setfps
        when (stateScreenshootIt newState) $ do
          takeScreenshoot saveFilename surface newState
        unless (shouldQuit || stateQuit state || stateScreenshootIt newState) $
          loop newState

  loop initialState

  freeSurface surface
  destroyRenderer renderer
  destroyWindow window
  quit
  where
    int i = fromIntegral i :: Int
    cint i = fromIntegral i :: CInt
