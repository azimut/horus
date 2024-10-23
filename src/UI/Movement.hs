{-# LANGUAGE RecordWildCards #-}

module UI.Movement (moveRight, moveLeft, moveUp, moveDown) where

import Foreign.C (CInt)
import UI.State (State (..))

moveStep :: CInt
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

cint :: (Integral a) => a -> CInt
cint i = fromIntegral i :: CInt
