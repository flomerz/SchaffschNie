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

import Game.AppTypes (WindowSize)
import Game.Output.Types
import Game.Output.Shapes
import Game.Output.Renderer


init :: WindowSize -> String -> IO GraphicsEnv
init winSize@(winWidth, winHeight) title = do
    SDL.initialize [SDL.InitVideo]
    Font.init

    let windowConf = SDL.defaultWindow { SDL.windowInitialSize = V2 (fromIntegral winWidth) (fromIntegral winHeight) }
    window <- SDL.createWindow (pack title) windowConf

    SDL.showWindow window

    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound 0

    graphicImages <- loadImages renderer

    return (winSize, window, renderer, graphicImages)

    where
        loadImages :: SDL.Renderer -> IO GraphicImages
        loadImages renderer = do
            imgPlayer <- loadImage "res/imgs/player.bmp"
            imgAir    <- loadImage "res/imgs/box.bmp"
            imgBox    <- loadImage "res/imgs/box.bmp"
            imgLava   <- loadImage "res/imgs/lava.bmp"
            return $ GraphicImages imgPlayer imgAir imgBox imgLava
            where loadImage file = do
                    imageSurface <- SDL.loadBMP file
                    imageTexture <- SDL.createTextureFromSurface renderer imageSurface
                    return (imageTexture, imageSurface)


quit :: GraphicsEnv -> IO ()
quit (_, window, renderer, graphicImages) = do
    destroyImages graphicImages
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    Font.quit
    SDL.quit
    where
        destroyImages :: GraphicImages -> IO ()
        destroyImages (GraphicImages imgPlayer imgAir imgBox imgLava) = do
            destroyImage imgPlayer
            destroyImage imgAir
            destroyImage imgBox
            destroyImage imgLava
            where destroyImage (imageTexture, imageSurface) = do
                    SDL.destroyTexture imageTexture
                    SDL.freeSurface imageSurface


output :: GraphicsEnv -> RenderObject -> IO ()
output env@(_, _, renderer, _) obj = SDL.clear renderer >> render env obj >> SDL.present renderer
