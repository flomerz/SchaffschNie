module Output.Graphics
    ( init
    , quit
    , render
    ) where

import Prelude hiding (init)


import qualified SDL
import qualified Graphics.UI.SDL.TTF as Font

import Control.Arrow
import Data.Text (pack)
import Data.StateVar (($=))
import Data.Colour.SRGB (toSRGB24, RGB(..))
import Linear (V2(..), V4(..))
import Linear.Affine (Point(..))

import Output.Shapes

type WindowDim = (Int, Int)
type GraphicsEnv = (SDL.Window, SDL.Renderer)

init :: WindowDim -> String -> IO GraphicsEnv
init (winWidth, winHeight) title = do
    SDL.initialize [SDL.InitVideo]
    Font.init

    let windowConf = SDL.defaultWindow { SDL.windowInitialSize = V2 (fromIntegral winWidth) (fromIntegral winHeight) }
    window <- SDL.createWindow (pack title) windowConf
    
    SDL.showWindow window

    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

    return (window, renderer)


quit :: GraphicsEnv -> IO ()
quit (window, renderer) = do
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    Font.quit
    SDL.quit

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
