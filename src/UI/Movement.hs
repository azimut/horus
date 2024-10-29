{-# LANGUAGE RecordWildCards #-}

module UI.Movement (updateMovement) where

import Foreign.C.Types (CDouble (..))
import GHC.Float (double2Int, float2Double)
import SDL (Scancode, V2 (..), (^*))
import qualified SDL
import UI.State (State (..))

deceleration :: Double
deceleration = 1

pushBy :: Double
pushBy = 5

updateMovement :: (Scancode -> Bool) -> State -> State
updateMovement keysState = applyForce . addForce keysState

applyForce :: State -> State
applyForce state@State {..} =
  let maxX = fromIntegral stateTextureWidth - stateZoomWidth
      maxY = fromIntegral stateTextureHeight - stateZoomHeight
      (V2 offX offY) = stateOrigin
      (V2 velX velY) = stateVel
      radians = toRadians stateRotation
      newX = max 0 $ min maxX $ offX + fromIntegral (double2Int (realToFrac (CDouble velX * cos radians - CDouble velY * sin radians)))
      newY = max 0 $ min maxY $ offY + fromIntegral (double2Int (realToFrac (CDouble velX * sin radians + CDouble velY * cos radians)))
   in state
        { stateOrigin = V2 newX newY,
          stateVel =
            decelerate
              ( V2
                  -- NOTE: when touching the walls, zero out velocity
                  (if newX == maxX || newX == 0 then 0 else velX)
                  (if newY == maxY || newY == 0 then 0 else velY)
              )
        }

addForce :: (Scancode -> Bool) -> State -> State
addForce keysState state@State {..} =
  let attenuation = max 0.25 (float2Double stateZoomBy)
   in state
        { stateVel =
            stateVel + pushIt keysState ^* attenuation
        }

pushIt :: (Scancode -> Bool) -> V2 Double
pushIt f = pushUp f + pushDown f + pushLeft f + pushRight f

pushUp :: (Scancode -> Bool) -> V2 Double
pushUp keysState
  | keysState SDL.ScancodeUp || keysState SDL.ScancodeK = V2 0 (-pushBy)
  | otherwise = V2 0 0

pushDown :: (Scancode -> Bool) -> V2 Double
pushDown keysState
  | keysState SDL.ScancodeDown || keysState SDL.ScancodeJ = V2 0 pushBy
  | otherwise = V2 0 0

pushRight :: (Scancode -> Bool) -> V2 Double
pushRight keysState
  | keysState SDL.ScancodeRight || keysState SDL.ScancodeL = V2 pushBy 0
  | otherwise = V2 0 0

pushLeft :: (Scancode -> Bool) -> V2 Double
pushLeft keysState
  | keysState SDL.ScancodeLeft || keysState SDL.ScancodeH = V2 (-pushBy) 0
  | otherwise = V2 0 0

decelerate :: V2 Double -> V2 Double
decelerate = fmap toZero

toZero :: Double -> Double
toZero n =
  case compare n 0 of
    LT -> n + deceleration
    GT -> n - deceleration
    EQ -> 0

toRadians :: (Floating a) => a -> a
toRadians n = n * (pi / 180)
