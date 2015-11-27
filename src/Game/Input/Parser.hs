module Game.Input.Parser
    ( parse
    ) where

import Prelude hiding
        ( Left
        , Right
        )

import qualified SDL

import Game.Input.Events


parse :: Maybe SDL.EventPayload -> InputEvent
parse payload = case payload of
        Just SDL.QuitEvent              -> Quit

        Just (SDL.KeyboardEvent ev)     -> case SDL.keyboardEventKeyMotion ev of
                                            SDL.Pressed     -> Key $ parseKey ev
                                            SDL.Released    -> Key Nothing

        Just (SDL.MouseButtonEvent ev)  -> case (SDL.mouseButtonEventMotion ev, SDL.mouseButtonEventButton ev) of
                                            (SDL.Pressed, SDL.ButtonLeft)   -> Mouse $ Just Left
                                            (SDL.Pressed, SDL.ButtonRight)  -> Mouse $ Just Right
                                            (SDL.Released, _)               -> Mouse Nothing
                                            _                               -> NoInput

        _                               -> NoInput

parseKey :: SDL.KeyboardEventData -> KeyEvent
parseKey ev = case code of
                SDL.ScancodeSpace   -> Just KeySpace
                SDL.Scancode1       -> Just Key1
                SDL.Scancode2       -> Just Key2
                SDL.Scancode3       -> Just Key3
                _                   -> Nothing
        where code = SDL.keysymScancode $ SDL.keyboardEventKeysym ev
