{-# LANGUAGE StrictData #-}

module UI.State (emptyState, State (..)) where

import Foreign.C (CDouble, CInt)

data State = State
  { stateRotation :: CDouble,
    stateQuit :: Bool,
    stateVFlip :: Bool,
    stateHFlip :: Bool,
    stateTextureWidth :: Int,
    stateTextureHeight :: Int,
    stateZoomWidth :: CInt,
    stateZoomHeight :: CInt,
    stateOffsetX :: CInt,
    stateOffsetY :: CInt,
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
