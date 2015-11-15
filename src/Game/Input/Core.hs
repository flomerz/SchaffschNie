module Game.Input.Core
    ( input
    , getTime
    , module Game.Input.Parser
    , module Game.Input.Events
    ) where

import qualified SDL

import Game.Input.Events
import Game.Input.Parser


input :: IO InputEvent
input = do
        sdlEvent <- SDL.pollEvent
        return $ parse $ SDL.eventPayload <$> sdlEvent

getTime :: Fractional a => IO a
getTime = SDL.time
