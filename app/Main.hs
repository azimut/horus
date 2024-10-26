{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Foreign.C (CInt)
import SDL
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import UI.Draw (draw)
import UI.Events (updateEvents)
import UI.Shoot (surfaceFromPointer, takeScreenshoot)
import UI.State (State (..), newState)
import Win (RawImage (..), loadScreenshoot)

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
  let initialState = newState (textureWidth textureInfo) (textureHeight textureInfo)
  draw renderer texture initialState
  present renderer

  SDL.rendererDrawColor renderer $= V4 0 0 0 0

  let loop state = do
        events <- map SDL.eventPayload <$> SDL.pollEvents
        let shouldQuit = SDL.QuitEvent `elem` events
            updatedState = updateEvents state events
        clear renderer
        draw renderer texture updatedState
        present renderer
        setfps
        when (stateScreenshootIt updatedState) $ do
          homeDir <- getHomeDirectory
          takeScreenshoot (homeDir </> "horus.png") surface updatedState
        unless (shouldQuit || stateQuit updatedState || stateScreenshootIt updatedState) $
          loop updatedState

  loop initialState

  freeSurface surface
  destroyRenderer renderer
  destroyWindow window
  quit
  where
    cint i = fromIntegral i :: CInt
