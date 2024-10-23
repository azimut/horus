module UI.Events (updateEvents, rotateIt, flipIt, isShifting) where

import Data.Fixed (mod')
import SDL
import UI.Movement
import UI.State (State (..))
import UI.Zoom

updateEvents :: State -> [EventPayload] -> State
updateEvents = foldr updateEvent

updateEvent :: EventPayload -> State -> State
updateEvent (SDL.KeyboardEvent event) state
  | SDL.keyboardEventKeyMotion event == SDL.Pressed =
      case SDL.keysymKeycode (SDL.keyboardEventKeysym event) of
        SDL.KeycodeS -> state {stateScreenshootIt = True}
        SDL.KeycodeF -> flipIt state event
        SDL.KeycodeQ -> state {stateQuit = True}
        SDL.KeycodeR -> rotateIt state event
        SDL.KeycodePlus -> zoomIn state
        SDL.KeycodeMinus -> zoomOut state
        SDL.KeycodeH -> moveLeft state
        SDL.KeycodeL -> moveRight state
        SDL.KeycodeJ -> moveDown state
        SDL.KeycodeK -> moveUp state
        _otherKey -> state
updateEvent _ state = state

isShifting :: KeyboardEventData -> Bool
isShifting event = SDL.keyModifierLeftShift keyModifier || SDL.keyModifierRightShift keyModifier
  where
    keyModifier = SDL.keysymModifier (SDL.keyboardEventKeysym event)

rotateIt :: State -> KeyboardEventData -> State
rotateIt state event =
  if isShifting event
    then state {stateRotation = mod' (stateRotation state - 45) 360}
    else state {stateRotation = mod' (stateRotation state + 45) 360}

flipIt :: State -> KeyboardEventData -> State
flipIt state event =
  if isShifting event
    then state {stateVFlip = not (stateVFlip state)}
    else state {stateHFlip = not (stateHFlip state)}
