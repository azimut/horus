{-# LANGUAGE RecordWildCards #-}

module UI.Movement (updateMovement) where

import GHC.Float (float2Int)
import SDL (Scancode, V2 (..))
import qualified SDL
import UI.State (State (..))

deceleration :: Float
deceleration = 1

pushBy :: Float
pushBy = 2.5

updateMovement :: (Scancode -> Bool) -> State -> State
updateMovement keysState state =
  applyForce $
    state
      { stateVel =
          stateVel state
            + pushUp keysState
            + pushDown keysState
            + pushLeft keysState
            + pushRight keysState
      }

pushUp :: (Scancode -> Bool) -> V2 Float
pushUp keysState
  | keysState SDL.ScancodeUp || keysState SDL.ScancodeK = V2 0 (-pushBy)
  | otherwise = V2 0 0

pushDown :: (Scancode -> Bool) -> V2 Float
pushDown keysState
  | keysState SDL.ScancodeDown || keysState SDL.ScancodeJ = V2 0 pushBy
  | otherwise = V2 0 0

pushRight :: (Scancode -> Bool) -> V2 Float
pushRight keysState
  | keysState SDL.ScancodeRight || keysState SDL.ScancodeL = V2 pushBy 0
  | otherwise = V2 0 0

pushLeft :: (Scancode -> Bool) -> V2 Float
pushLeft keysState
  | keysState SDL.ScancodeLeft || keysState SDL.ScancodeH = V2 (-pushBy) 0
  | otherwise = V2 0 0

applyForce :: State -> State
applyForce state@State {..} =
  let (V2 offX offY) = stateOrigin
      (V2 velX velY) = stateVel
      maxX = fromIntegral stateTextureWidth - stateZoomWidth
      maxY = fromIntegral stateTextureHeight - stateZoomHeight
      newX = max 0 $ min maxX $ offX + fromIntegral (float2Int velX)
      newY = max 0 $ min maxY $ offY + fromIntegral (float2Int velY)
   in state
        { stateOrigin = V2 newX newY,
          stateVel =
            decelerate
              ( V2
                  (if newX == maxX || newX == 0 then 0 else velX)
                  (if newY == maxY || newY == 0 then 0 else velY)
              )
        }

decelerate :: V2 Float -> V2 Float
decelerate v@(V2 0 0) = v
decelerate (V2 x y) = V2 (toZero x) (toZero y)

toZero :: Float -> Float
toZero n =
  case compare n 0 of
    LT -> n + deceleration
    GT -> n - deceleration
    EQ -> 0
