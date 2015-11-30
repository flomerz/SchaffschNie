module Game.Output.Types where

import Data.Map

import qualified SDL

type WindowSize = (Int, Int)

data GraphicsEnv = GraphicsEnv { gWindowSize :: WindowSize
                               , gWindow     :: SDL.Window
                               , gRenderer   :: SDL.Renderer
                               , gImages     :: Map String GraphicImage
                               }

type GraphicImage = (SDL.Texture, SDL.Surface)
