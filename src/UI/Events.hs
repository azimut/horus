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
        SDL.KeycodeLess -> if isShifting event then rotateRight state else rotateLeft state
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

rotateLeft :: State -> State
rotateLeft state =
  state
    { stateRotation = mod' (stateRotation state - 45) 360,
      stateVel = V2 0 0
    }

rotateRight :: State -> State
rotateRight state =
  state
    { stateRotation = mod' (stateRotation state + 45) 360,
      stateVel = V2 0 0
    }

flipIt :: State -> KeyboardEventData -> State
flipIt state event =
  if isShifting event
    then state {stateVFlip = not (stateVFlip state)}
    else state {stateHFlip = not (stateHFlip state)}
