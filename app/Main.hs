{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Fixed (mod')
import qualified Foreign.C.Types as F
import GHC.Float (float2Int, int2Float)
import SDL
import qualified SDL.Image as Image
import qualified Store as S
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
      defaultWindow {windowInitialSize = V2 640 360} -- 600 480
  SDL.showWindow window

  renderer <- createRenderer window (-1) defaultRenderer
  SDL.rendererDrawColor renderer $= V4 0 0 0 0

  surface <- Image.load "/home/sendai/test.png"
  texture <- createTextureFromSurface renderer surface
  SDL.freeSurface surface

  let loop state = do
        events <- map SDL.eventPayload <$> SDL.pollEvents
        let shouldQuit = SDL.QuitEvent `elem` events
            newState = updateEvents state events
        -- unless (null events) $ print events
        print [show $ stateZoomBy state, show $ stateZoomWidth state, show $ stateZoomHeight state]
        clear renderer
        draw renderer texture newState
        present renderer
        setfps
        unless (shouldQuit || stateQuit state) $
          loop newState

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

data State = State
  { stateRotation :: F.CDouble,
    stateQuit :: Bool,
    stateVFlip :: Bool,
    stateHFlip :: Bool,
    stateTextureWidth :: Int,
    stateTextureHeight :: Int,
    stateZoomWidth :: F.CInt,
    stateZoomHeight :: F.CInt,
    stateOffsetX :: F.CInt,
    stateOffsetY :: F.CInt,
    stateZoomBy :: Float
  }

emptyState :: State
emptyState =
  State
    { stateRotation = 0,
      stateQuit = False,
      stateVFlip = False,
      stateHFlip = False,
      stateOffsetX = 0,
      stateOffsetY = 0,
      stateTextureWidth = 0,
      stateTextureHeight = 0,
      stateZoomBy = 1,
      stateZoomHeight = 0,
      stateZoomWidth = 0
    }

draw :: Renderer -> Texture -> State -> IO ()
draw renderer texture State {..} =
  copyEx
    renderer
    texture
    ( Just
        ( Rectangle
            (P (V2 stateOffsetX stateOffsetY))
            (V2 stateZoomWidth stateZoomHeight)
        )
    )
    Nothing
    stateRotation
    Nothing
    (V2 stateHFlip stateVFlip)

updateEvents :: State -> [EventPayload] -> State
updateEvents = foldr updateEvent

updateEvent :: EventPayload -> State -> State
updateEvent (SDL.KeyboardEvent event) state
  | SDL.keyboardEventKeyMotion event == SDL.Pressed =
      case SDL.keysymKeycode (SDL.keyboardEventKeysym event) of
        SDL.KeycodeF ->
          if SDL.keyModifierLeftShift (SDL.keysymModifier (SDL.keyboardEventKeysym event))
            || SDL.keyModifierRightShift (SDL.keysymModifier (SDL.keyboardEventKeysym event))
            then state {stateVFlip = not (stateVFlip state)}
            else state {stateHFlip = not (stateHFlip state)}
        SDL.KeycodeQ -> state {stateQuit = True}
        SDL.KeycodeR ->
          let newRotation = mod' (stateRotation state + 90) 360
           in state {stateRotation = newRotation}
        -- Zoom
        SDL.KeycodePlus -> zoomIn state
        SDL.KeycodeMinus -> zoomOut state
        -- Movement Offset
        SDL.KeycodeH ->
          state {stateOffsetX = max 0 $ stateOffsetX state - 2}
        SDL.KeycodeL ->
          state {stateOffsetX = stateOffsetX state + 2}
        SDL.KeycodeJ ->
          state {stateOffsetY = stateOffsetY state + 2}
        SDL.KeycodeK ->
          state {stateOffsetY = max 0 $ stateOffsetY state - 2}
        _otherKey ->
          state
updateEvent _ state = state

float2CInt :: Float -> F.CInt
float2CInt n = fromIntegral (float2Int n) :: F.CInt

zoomIn :: State -> State
zoomIn state =
  let newZoomBy = max 0.1 $ stateZoomBy state - 0.1
      newZoomHeight = float2CInt $ int2Float (stateTextureHeight state) * newZoomBy
      newZoomWidth = float2CInt $ int2Float (stateTextureWidth state) * newZoomBy
      newOffsetX = float2CInt $ int2Float (stateTextureWidth state) * (1 - newZoomBy) * 0.5
      newOffsetY = float2CInt $ int2Float (stateTextureHeight state) * (1 - newZoomBy) * 0.5
   in state
        { stateZoomBy = newZoomBy,
          stateZoomHeight = newZoomHeight,
          stateZoomWidth = newZoomWidth,
          stateOffsetX = newOffsetX,
          stateOffsetY = newOffsetY
        }

zoomOut :: State -> State
zoomOut state =
  let newZoomBy = min 1.0 $ stateZoomBy state + 0.1
      newZoomHeight = float2CInt $ int2Float (stateTextureHeight state) * newZoomBy
      newZoomWidth = float2CInt $ int2Float (stateTextureWidth state) * newZoomBy
      maxHeight = cint (stateTextureHeight state) - stateOffsetY state
      maxWidth = cint (stateTextureWidth state) - stateOffsetX state
   in state
        { stateZoomBy = newZoomBy,
          stateZoomHeight = min maxHeight newZoomHeight,
          stateZoomWidth = min maxWidth newZoomWidth
        }
  where
    cint i = fromIntegral i :: F.CInt
