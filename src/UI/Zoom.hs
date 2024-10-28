{-# LANGUAGE RecordWildCards #-}

module UI.Zoom (zoomOut, zoomIn) where

import Foreign.C (CInt)
import GHC.Float (float2Int, int2Float)
import SDL (V2 (..))
import UI.State (State (..))

zoomStep :: Float
zoomStep = 0.1

zoomIn :: State -> State
zoomIn state@State {..} =
  let (V2 stateOriginX stateOriginY) = stateOrigin
      newZoomBy = max zoomStep $ stateZoomBy - zoomStep
      newOffsetX =
        stateOriginX
          + if stateZoomBy /= newZoomBy
            then multBy stateTextureWidth (zoomStep * 0.5)
            else 0
      newOffsetY =
        stateOriginY
          + if stateZoomBy /= newZoomBy
            then multBy stateTextureHeight (zoomStep * 0.5)
            else 0
   in state
        { stateZoomBy = newZoomBy,
          stateZoomHeight = multBy stateTextureHeight newZoomBy,
          stateZoomWidth = multBy stateTextureWidth newZoomBy,
          stateOrigin = V2 newOffsetX newOffsetY
        }

zoomOut :: State -> State
zoomOut state@State {..} =
  let zoomBy = min 1.0 $ stateZoomBy + zoomStep
      width = multBy stateTextureWidth zoomBy
      height = multBy stateTextureHeight zoomBy
      deltaOriginX = multBy stateTextureWidth (zoomStep * 0.5) -- constant+
      deltaOriginY = multBy stateTextureHeight (zoomStep * 0.5) -- constant+
      (V2 x y) = stateOrigin
      originX = x - deltaOriginX
      originY = y - deltaOriginY
      zoomWidth = width + (if originX < 0 then (-originX) else 0)
      zoomHeight = height + (if originY < 0 then (-originY) else 0)
      maxZoomWidth = fromIntegral stateTextureWidth - max 0 originX
      maxZoomHeight = fromIntegral stateTextureHeight - max 0 originY
   in state
        { stateZoomBy = zoomBy,
          stateZoomWidth = min zoomWidth maxZoomWidth,
          stateZoomHeight = min zoomHeight maxZoomHeight,
          stateOrigin = V2 (max 0 originX) (max 0 originY)
        }

multBy :: Int -> Float -> CInt
multBy n by = fromIntegral . float2Int $ int2Float n * by
