
module Keyboard where

import SDL

import Data.List (foldl')

import Data.Set (Set)
import qualified Data.Set as S

type Keyboard = Set Keycode

-- | création de la structure d'état de clavier (vide)
createKeyboard :: Keyboard
createKeyboard = S.empty

handleEvent :: Event -> Keyboard -> Keyboard
handleEvent event kbd =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      if keyboardEventKeyMotion keyboardEvent == Pressed
      then S.insert (keysymKeycode (keyboardEventKeysym keyboardEvent)) kbd
      else if keyboardEventKeyMotion keyboardEvent == Released
           then S.delete (keysymKeycode (keyboardEventKeysym keyboardEvent)) kbd
           else kbd
    _ -> kbd

-- | prise en compte des événements SDL2 pour mettre à jour l'état du clavier
handleEvents :: [Event] -> Keyboard -> Keyboard
handleEvents events kbd = foldl' (flip handleEvent) kbd events

-- | Vérifies sir le *keycode* spécificé est actuellement
-- | actif sur le clavier.
keypressed :: Keycode -> Keyboard -> Bool
keypressed kc kbd = S.member kc kbd
