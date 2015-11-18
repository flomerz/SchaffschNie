module Game.Output.Types where

import qualified SDL

import Game.AppTypes (WindowSize)


type GraphicsEnv = (WindowSize, SDL.Window, SDL.Renderer, GraphicImages)

data GraphicImages = GraphicImages { imagePlayer    :: (SDL.Texture, SDL.Surface)
                                   , imageAir       :: (SDL.Texture, SDL.Surface)
                                   , imageBox       :: (SDL.Texture, SDL.Surface)
                                   , imageLava      :: (SDL.Texture, SDL.Surface)
                                   }
