{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Data.Fixed (mod')
import qualified Foreign.C as F
import SDL
import qualified SDL.Image as Image
import qualified Store as S
import UI.Shoot
import UI.State
import UI.Zoom
import Win

setfps :: IO ()
setfps = threadDelay 16_000 -- 60fps

main :: IO ()
main = do
  -- Screenshot before drawing SDL window
  rootImage <- loadScreenshoot
  S.savePng rootImage "/home/sendai/test.png"

  initializeAll

  window <-
    createWindow
      "Hello SDL"
      defaultWindow {windowInitialSize = V2 640 360} -- 640 360 / 600 480
  SDL.showWindow window

  renderer <- createRenderer window (-1) defaultRenderer
  SDL.rendererDrawColor renderer $= V4 0 0 0 0

  surface <- Image.load "/home/sendai/test.png"
  texture <- createTextureFromSurface renderer surface

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
        unless (shouldQuit || stateQuit state) $
          loop newState {stateScreenshootIt = False}

  textureInfo <- queryTexture texture
  loop
    emptyState
      { stateTextureHeight = int (textureHeight textureInfo),
        stateTextureWidth = int (textureWidth textureInfo),
        stateZoomWidth = textureWidth textureInfo,
        stateZoomHeight = textureHeight textureInfo
      }

  destroyRenderer renderer
  destroyWindow window
  quit
  where
    int i = fromIntegral i :: Int

updateEvents :: State -> [EventPayload] -> State
updateEvents = foldr updateEvent

isShifting :: KeyboardEventData -> Bool
isShifting event = SDL.keyModifierLeftShift keyModifier || SDL.keyModifierRightShift keyModifier
  where
    keyModifier = SDL.keysymModifier (SDL.keyboardEventKeysym event)

rotateIt :: State -> KeyboardEventData -> State
rotateIt state event =
  if isShifting event
    then state {stateRotation = mod' (stateRotation state - 45) 360}
    else state {stateRotation = mod' (stateRotation state + 45) 360}

flipIt :: State -> KeyboardEventData -> State
flipIt state event =
  if isShifting event
    then state {stateVFlip = not (stateVFlip state)}
    else state {stateHFlip = not (stateHFlip state)}

updateEvent :: EventPayload -> State -> State
updateEvent (SDL.KeyboardEvent event) state
  | SDL.keyboardEventKeyMotion event == SDL.Pressed =
      case SDL.keysymKeycode (SDL.keyboardEventKeysym event) of
        SDL.KeycodeS -> state {stateScreenshootIt = True}
        SDL.KeycodeF -> flipIt state event
        SDL.KeycodeQ -> state {stateQuit = True}
        SDL.KeycodeR -> rotateIt state event
        SDL.KeycodePlus -> zoomIn state
        SDL.KeycodeMinus -> zoomOut state
        SDL.KeycodeH -> moveLeft state
        SDL.KeycodeL -> moveRight state
        SDL.KeycodeJ -> moveDown state
        SDL.KeycodeK -> moveUp state
        _otherKey -> state
updateEvent _ state = state

cint :: (Integral a) => a -> F.CInt
cint i = fromIntegral i :: F.CInt

moveStep :: F.CInt
moveStep = 4

moveLeft :: State -> State
moveLeft state = state {stateOffsetX = max 0 $ stateOffsetX state - moveStep}

moveRight :: State -> State
moveRight state@State {..} =
  let canMove = max 0 $ cint stateTextureWidth - (stateOffsetX + cint stateZoomWidth)
   in state {stateOffsetX = stateOffsetX + min moveStep canMove}

moveDown :: State -> State
moveDown state@State {..} =
  let canMove = max 0 $ cint stateTextureHeight - (stateOffsetY + cint stateZoomHeight)
   in state {stateOffsetY = stateOffsetY + min moveStep canMove}

moveUp :: State -> State
moveUp state = state {stateOffsetY = max 0 $ stateOffsetY state - moveStep}
