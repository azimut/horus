{-# LANGUAGE RecordWildCards #-}

module UI.Draw (draw) where

import SDL
import UI.State (State (..))

draw :: Renderer -> Texture -> State -> IO ()
draw renderer texture State {..} =
  copyEx
    renderer
    texture
    ( Just
        ( Rectangle
            (P stateOffset)
            (V2 stateZoomWidth stateZoomHeight)
        )
    )
    Nothing
    stateRotation
    Nothing
    (V2 stateHFlip stateVFlip)
