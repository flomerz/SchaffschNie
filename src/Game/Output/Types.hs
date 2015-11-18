module Game.Output.Types where

import qualified SDL

import Game.AppTypes (WindowSize)


type GraphicsEnv = (WindowSize, SDL.Window, SDL.Renderer)
