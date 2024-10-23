{-# LANGUAGE RecordWildCards #-}

module UI.Zoom (zoomOut, zoomIn) where

import Foreign.C (CInt)
import GHC.Float (float2Int, int2Float)
import UI.State (State (..))

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

float2CInt :: Float -> CInt
float2CInt n = fromIntegral (float2Int n) :: CInt

cint :: (Integral a) => a -> CInt
cint i = fromIntegral i :: CInt
