module Game.Output.Core
    ( init
    , quit
    , output
    , module Game.Output.Types
    , module Game.Output.Shapes
    , module Game.Output.Renderer
    ) where

import Prelude hiding (init)

import Data.Text (pack)
import Data.StateVar (($=))
import Linear (V2(..), V4(..))

import qualified SDL
import qualified Graphics.UI.SDL.TTF as Font

import Game.Output.Types
import Game.Output.Shapes
import Game.Output.Renderer


init :: WindowDim -> String -> IO GraphicsEnv
init (winWidth, winHeight) title = do
    SDL.initialize [SDL.InitVideo]
    Font.init

    let windowConf = SDL.defaultWindow { SDL.windowInitialSize = V2 (fromIntegral winWidth) (fromIntegral winHeight) }
    window <- SDL.createWindow (pack title) windowConf

    SDL.showWindow window

    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound 0

    return (window, renderer)


quit :: GraphicsEnv -> IO ()
quit (window, renderer) = do
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    Font.quit
    SDL.quit

output :: GraphicsEnv -> RenderObject -> IO ()
output env@(_,renderer) obj = SDL.clear renderer >> render env obj >> SDL.present renderer
