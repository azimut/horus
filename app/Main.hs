{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Foreign.C (CInt)
import SDL
import Storage (savePath)
import UI.Draw (draw)
import UI.Events (updateEvents)
import UI.Movement (updateMovement)
import UI.Shoot (surfaceFromPointer, takeScreenshoot)
import UI.State (State (..), newState)
import Win (RawImage (..), loadScreenshoot)

setfps :: IO ()
setfps = threadDelay 32_000 -- 60fps 16k

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

  loop initialState renderer surface texture

  freeSurface surface
  destroyRenderer renderer
  destroyWindow window
  quit
  where
    cint i = fromIntegral i :: CInt

loop :: State -> Renderer -> Surface -> Texture -> IO ()
loop state renderer surface texture = do
  events <- map SDL.eventPayload <$> SDL.pollEvents
  keysState <- getKeyboardState
  let shouldQuit = SDL.QuitEvent `elem` events
      updatedState = updateMovement keysState $ updateEvents state events
  clear renderer
  draw renderer texture updatedState
  present renderer
  setfps
  when (stateScreenshootIt updatedState) $ do
    path <- savePath
    takeScreenshoot path surface updatedState
    putStrLn $ "Image saved at: " <> path
  unless (shouldQuit || stateQuit updatedState || stateScreenshootIt updatedState) $
    loop updatedState renderer surface texture
