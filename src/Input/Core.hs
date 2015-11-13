module Input.Core
    ( input
    , getTime
    , module Input.Parser
    ) where

import qualified SDL

import Input.Events
import Input.Parser


input :: IO InputEvent
input = do 
        sdlEvent <- SDL.pollEvent
        return $ parse $ SDL.eventPayload <$> sdlEvent

getTime :: Fractional a => IO a
getTime = SDL.time
