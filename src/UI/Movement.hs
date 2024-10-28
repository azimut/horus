{-# LANGUAGE RecordWildCards #-}

module UI.Movement (updateMovement) where

import Foreign.C (CInt)
import SDL (Scancode, V2 (..))
import qualified SDL
import UI.State (State (..))

deceleration :: CInt
deceleration = 1

pushStep :: CInt
pushStep = 3

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

pushUp :: (Scancode -> Bool) -> V2 CInt
pushUp keysState
  | keysState SDL.ScancodeUp || keysState SDL.ScancodeK = V2 0 (-pushStep)
  | otherwise = V2 0 0

pushDown :: (Scancode -> Bool) -> V2 CInt
pushDown keysState
  | keysState SDL.ScancodeDown || keysState SDL.ScancodeJ = V2 0 pushStep
  | otherwise = V2 0 0

pushRight :: (Scancode -> Bool) -> V2 CInt
pushRight keysState
  | keysState SDL.ScancodeRight || keysState SDL.ScancodeL = V2 pushStep 0
  | otherwise = V2 0 0

pushLeft :: (Scancode -> Bool) -> V2 CInt
pushLeft keysState
  | keysState SDL.ScancodeLeft || keysState SDL.ScancodeH = V2 (-pushStep) 0
  | otherwise = V2 0 0

applyForce :: State -> State
applyForce state@State {..} =
  let (V2 offX offY) = stateOrigin
      (V2 velX velY) = stateVel
      maxX = fromIntegral stateTextureWidth - stateZoomWidth
      maxY = fromIntegral stateTextureHeight - stateZoomHeight
      newX = max 0 $ min maxX $ offX + velX
      newY = max 0 $ min maxY $ offY + velY
   in state
        { stateOrigin = V2 newX newY,
          stateVel = decelerate stateVel
        }

decelerate :: V2 CInt -> V2 CInt
decelerate v@(V2 0 0) = v
decelerate (V2 x y) = V2 (toZero x) (toZero y)

toZero :: CInt -> CInt
toZero n =
  case compare n 0 of
    LT -> n + deceleration
    GT -> n - deceleration
    EQ -> 0
