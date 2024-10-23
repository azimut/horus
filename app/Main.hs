{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless, void, when)
import Data.Fixed (mod')
import qualified Foreign as F
import qualified Foreign.C as F
import GHC.Float (float2Int, int2Float)
import SDL
import qualified SDL.Image as Image
import qualified SDL.Internal.Types as SINT
import qualified SDL.Raw.Enum as SENUM
import qualified SDL.Raw.Video as SRAW
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
          takeScreenshoot "/home/sendai/new.bmp" surface state
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
    stateZoomBy :: Float,
    stateScreenshootIt :: Bool
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
      stateZoomWidth = 0,
      stateScreenshootIt = False
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

float2CInt :: Float -> F.CInt
float2CInt n = fromIntegral (float2Int n) :: F.CInt

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

zoomStep :: Float
zoomStep = 0.1

zoomIn :: State -> State
zoomIn state@State {..} =
  let newZoomBy = max zoomStep $ stateZoomBy - zoomStep
      newZoomHeight = float2CInt $ int2Float stateTextureHeight * newZoomBy
      newZoomWidth = float2CInt $ int2Float stateTextureWidth * newZoomBy
      newOffsetX =
        stateOffsetX
          + if stateZoomBy /= newZoomBy
            then float2CInt (int2Float stateTextureWidth * zoomStep * 0.5)
            else 0
      newOffsetY =
        stateOffsetY
          + if stateZoomBy /= newZoomBy
            then float2CInt (int2Float stateTextureHeight * zoomStep * 0.5)
            else 0
   in state
        { stateZoomBy = newZoomBy,
          stateZoomHeight = newZoomHeight,
          stateZoomWidth = newZoomWidth,
          stateOffsetX = newOffsetX,
          stateOffsetY = newOffsetY
        }

zoomOut :: State -> State
zoomOut state@State {..} =
  let newZoomBy = min 1.0 $ stateZoomBy + zoomStep
      newZoomHeight = float2CInt $ int2Float stateTextureHeight * newZoomBy
      newZoomWidth = float2CInt $ int2Float stateTextureWidth * newZoomBy
      newOffsetX = stateOffsetX - float2CInt (int2Float stateTextureWidth * zoomStep * 0.5)
      newOffsetY = stateOffsetY - float2CInt (int2Float stateTextureHeight * zoomStep * 0.5)
      maxHeight = cint stateTextureHeight - newOffsetY
      maxWidth = cint stateTextureWidth - newOffsetX
   in state
        { stateZoomBy = newZoomBy,
          stateZoomHeight = min maxHeight newZoomHeight,
          stateZoomWidth = min maxWidth newZoomWidth,
          stateOffsetX = max 0 newOffsetX,
          stateOffsetY = max 0 newOffsetY
        }

withRGBSurface :: V2 F.CInt -> PixelFormat -> (Surface -> IO ()) -> IO ()
withRGBSurface dimensions format =
  bracket (createRGBSurface dimensions format) freeSurface

withSRenderer :: Surface -> (Renderer -> IO ()) -> IO ()
withSRenderer surface =
  bracket (createSoftwareRenderer surface) destroyRenderer

takeScreenshoot :: FilePath -> Surface -> State -> IO ()
takeScreenshoot filename texSurface state = do
  let (width, height) = (stateZoomWidth state, stateZoomHeight state)
  putStrLn "Screenshooting...."
  withRGBSurface (V2 width height) ARGB8888 $ \surface ->
    withSRenderer surface $ \renderer -> do
      tmpTexture <- createTextureFromSurface renderer texSurface
      draw renderer tmpTexture state
      pixels <- surfacePixels surface
      void $ SRAW.renderReadPixels (r renderer) F.nullPtr SENUM.SDL_PIXELFORMAT_ARGB8888 pixels (width * 4)
      F.withCString filename $ \cFilename ->
        void $ SRAW.saveBMP (s surface) cFilename
  putStrLn "Done!"
  where
    r (SINT.Renderer rr) = rr
    s (Surface ss _) = ss
