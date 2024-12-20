{-# LANGUAGE RecordWildCards #-}

module UI.Events (updateEvents) where

import Data.Fixed (mod')
import SDL
import UI.State (State (..), resetState)
import UI.Zoom (zoomIn, zoomOut)

updateEvents :: State -> [EventPayload] -> State
updateEvents = foldr updateEvent

updateEvent :: EventPayload -> State -> State
updateEvent (SDL.KeyboardEvent event) state
  | SDL.keyboardEventKeyMotion event == SDL.Pressed =
      case SDL.keysymKeycode (SDL.keyboardEventKeysym event) of
        SDL.KeycodeQ -> state {stateQuit = True}
        SDL.KeycodeS -> state {stateScreenshootIt = True}
        SDL.KeycodeF -> flipIt state event
        SDL.KeycodePlus -> zoomIn state
        SDL.KeycodeMinus -> zoomOut state
        SDL.KeycodeR -> resetState state
        SDL.KeycodeLess -> rotateIt state event
        _otherKey -> state
updateEvent (SDL.MouseWheelEvent event) state =
  case SDL.mouseWheelEventPos event of
    (V2 0 1) -> zoomIn state
    (V2 0 (-1)) -> zoomOut state
    _anyPos -> error "wacky mouse-wheel value"
updateEvent _ state = state

isShifting :: KeyboardEventData -> Bool
isShifting event = SDL.keyModifierLeftShift keyModifier || SDL.keyModifierRightShift keyModifier
  where
    keyModifier = SDL.keysymModifier (SDL.keyboardEventKeysym event)

rotateIt :: State -> KeyboardEventData -> State
rotateIt state@State {..} event =
  if isShifting event
    then state {stateRotation = mod' (stateRotation + 45) 360, stateVel = V2 0 0}
    else state {stateRotation = mod' (stateRotation - 45) 360, stateVel = V2 0 0}

flipIt :: State -> KeyboardEventData -> State
flipIt state@State {..} event =
  if isShifting event
    then state {stateVFlip = not stateVFlip, stateVel = V2 0 0}
    else state {stateHFlip = not stateHFlip, stateVel = V2 0 0}
