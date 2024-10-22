{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Fixed (mod')
import qualified Foreign.C.Types as F
import GHC.Base (divInt)
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
      defaultWindow {windowInitialSize = V2 600 480} -- 600 480
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
        clear renderer
        draw renderer texture newState
        present renderer
        setfps
        unless (shouldQuit || stateQuit state) $
          loop newState

  textureInfo <- queryTexture texture
  loop
    emptyState
      { stateZoomOffsetMax =
          fromIntegral
            ( (fromIntegral (textureWidth textureInfo) :: Int) `divInt` 2
            ) ::
            F.CInt,
        stateTextureHeight = textureHeight textureInfo,
        stateTextureWidth = textureWidth textureInfo
      }

  destroyRenderer renderer
  destroyWindow window
  quit

data State = State
  { stateRotation :: Double,
    stateQuit :: Bool,
    stateVFlip :: Bool,
    stateHFlip :: Bool,
    stateTextureWidth :: F.CInt,
    stateTextureHeight :: F.CInt,
    stateZoomOffsetMax :: F.CInt,
    stateZoomOffset :: F.CInt
  }

emptyState :: State
emptyState =
  State
    { stateRotation = 0,
      stateQuit = False,
      stateVFlip = False,
      stateHFlip = False,
      stateZoomOffset = 0,
      stateZoomOffsetMax = 0,
      stateTextureWidth = 0,
      stateTextureHeight = 0
    }

draw :: Renderer -> Texture -> State -> IO ()
draw renderer texture State {..} =
  copyEx
    renderer
    texture
    ( Just
        ( Rectangle
            (P (V2 stateZoomOffset stateZoomOffset))
            (V2 (stateTextureWidth - stateZoomOffset * 2) (stateTextureHeight - stateZoomOffset * 2))
        )
    )
    Nothing
    (F.CDouble stateRotation)
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
        SDL.KeycodePlus -> state {stateZoomOffset = min (stateZoomOffsetMax state) $ stateZoomOffset state + 10}
        SDL.KeycodeMinus -> state {stateZoomOffset = max 0 $ stateZoomOffset state - 10}
        _otherKey ->
          state
updateEvent _ state = state
