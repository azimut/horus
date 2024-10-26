{-# LANGUAGE StrictData #-}

module UI.State (newState, resetState, State (..)) where

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
  deriving (Show)

newState :: (Integral a) => a -> a -> State
newState width height =
  State
    { stateRotation = 0,
      stateQuit = False,
      stateVFlip = False,
      stateHFlip = False,
      stateOffsetX = 0,
      stateOffsetY = 0,
      stateTextureWidth = fromIntegral width,
      stateTextureHeight = fromIntegral height,
      stateZoomBy = 1,
      stateZoomHeight = fromIntegral height,
      stateZoomWidth = fromIntegral width,
      stateScreenshootIt = False
    }

resetState :: State -> State
resetState state =
  state
    { stateZoomBy = 1,
      stateZoomWidth = fromIntegral (stateTextureWidth state),
      stateZoomHeight = fromIntegral (stateTextureHeight state),
      stateRotation = 0,
      stateOffsetX = 0,
      stateOffsetY = 0
    }
