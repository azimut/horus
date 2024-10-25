{-# LANGUAGE StrictData #-}

module UI.State (newState, State (..)) where

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
      stateTextureWidth = int width,
      stateTextureHeight = int height,
      stateZoomBy = 1,
      stateZoomHeight = cint height,
      stateZoomWidth = cint width,
      stateScreenshootIt = False
    }
  where
    int i = fromIntegral i :: Int
    cint i = fromIntegral i :: CInt
