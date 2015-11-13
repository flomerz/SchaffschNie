module Output.Core
    ( init
    , quit
    , output
    , module Output.Renderer
    , module Output.Shapes
    ) where

import Prelude hiding (init)


import qualified SDL
import qualified Graphics.UI.SDL.TTF as Font

import Data.Text (pack)
import Data.StateVar (($=))
import Linear (V2(..), V4(..))

import Output.Renderer
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

output :: SDL.Renderer -> RenderObject -> IO ()
output renderer obj = render renderer obj >> SDL.present renderer
