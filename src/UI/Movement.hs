{-# LANGUAGE RecordWildCards #-}

module UI.Movement (pushUp, pushRight, pushDown, pushLeft, move) where

import Foreign.C (CInt)
import SDL (Additive (..), V2 (..))
import UI.State (State (..))

deceleration :: CInt
deceleration = 1

pushStep :: CInt
pushStep = 3

pushLeft :: State -> State
pushLeft state = state {stateVel = stateVel state ^-^ V2 pushStep 0}

pushRight :: State -> State
pushRight state = state {stateVel = stateVel state ^+^ V2 pushStep 0}

pushUp :: State -> State
pushUp state = state {stateVel = stateVel state ^-^ V2 0 pushStep}

pushDown :: State -> State
pushDown state = state {stateVel = stateVel state ^+^ V2 0 pushStep}

move :: State -> State
move state@State {..} =
  let (V2 offX offY) = stateOffset
      (V2 velX velY) = stateVel
      maxX = fromIntegral stateTextureWidth - stateZoomWidth
      maxY = fromIntegral stateTextureHeight - stateZoomHeight
      newX = max 0 $ min maxX $ offX + velX
      newY = max 0 $ min maxY $ offY + velY
   in state
        { stateOffset = V2 newX newY,
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
