module Output.Renderer (render) where

import Prelude hiding (init)


import qualified SDL
-- import qualified Graphics.UI.SDL.TTF as Font

import Control.Arrow
import Data.StateVar (($=))
import Data.Colour.SRGB (toSRGB24, RGB(..))
import Linear (V2(..), V4(..))
import Linear.Affine (Point(..))

import Output.Shapes

render :: SDL.Renderer -> RenderObject -> IO ()
render renderer (RenderObject (Multiple objects) _ _) = SDL.clear renderer >> mapM_ (render renderer) objects
render renderer obj = do
    let (RGB r g b) = toSRGB24 $ objColour obj
    SDL.rendererDrawColor renderer $= V4 r g b maxBound

    let (px, py) = objPos >>> (\(x, y) -> (floor x, 300 - floor y)) $ obj
    
    case objShape $ objType obj of
        Rectangle size -> do
                let (sx, sy) = (\(x,y) -> (floor x, floor $ -y)) size
                let rectangle = Just . SDL.Rectangle (P $ mkv2 px py) $ mkv2 sx sy
                SDL.fillRect renderer rectangle

    SDL.present renderer

mkv2 :: Enum a => Int -> Int -> V2 a
mkv2 x y = (V2 (toEnum x) (toEnum y))
