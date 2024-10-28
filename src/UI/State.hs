{-# LANGUAGE StrictData #-}

module UI.State (newState, resetState, State (..)) where

import Foreign.C (CDouble, CInt)
import SDL (V2 (..))

data State = State
  { stateRotation :: CDouble,
    stateQuit :: Bool,
    stateVFlip :: Bool,
    stateHFlip :: Bool,
    stateTextureWidth :: Int,
    stateTextureHeight :: Int,
    stateZoomWidth :: CInt,
    stateZoomHeight :: CInt,
    stateZoomBy :: Float,
    stateOrigin :: V2 CInt,
    stateScreenshootIt :: Bool
    stateVel :: V2 Float,
  }
  deriving (Show)

newState :: (Integral a) => a -> a -> State
newState width height =
  State
    { stateRotation = 0,
      stateQuit = False,
      stateVFlip = False,
      stateHFlip = False,
      stateOrigin = V2 0 0,
      stateTextureWidth = fromIntegral width,
      stateTextureHeight = fromIntegral height,
      stateZoomBy = 1,
      stateZoomHeight = fromIntegral height,
      stateZoomWidth = fromIntegral width,
      stateVel = V2 0 0,
      stateScreenshootIt = False
    }

resetState :: State -> State
resetState state =
  state
    { stateZoomBy = 1,
      stateZoomWidth = fromIntegral (stateTextureWidth state),
      stateZoomHeight = fromIntegral (stateTextureHeight state),
      stateRotation = 0,
      stateOrigin = V2 0 0,
      stateVel = V2 0 0
    }
