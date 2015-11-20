module Game.Output.Core
    ( init
    , quit
    , output
    , module Game.Output.Types
    , module Game.Output.Shapes
    , module Game.Output.Renderer
    ) where

import Prelude hiding (init)

import System.Directory
import Data.Map (Map, fromList, elems)
import Control.Monad
import Control.Concurrent
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

    return $ GraphicsEnv winSize window renderer graphicImages

    where
        loadImages :: SDL.Renderer -> IO (Map String GraphicImage)
        loadImages renderer = do
            imageFiles <- getDirectoryContents "res/imgs/"
            images <- mapM loadImage $ filter (\imageFile -> imageFile /= ".." && imageFile /= ".") imageFiles
            return $ fromList images
            where
                loadImage :: FilePath -> IO (String, GraphicImage)
                loadImage filename = do
                    let name = takeWhile (/='.') filename
                    imageSurface <- SDL.loadBMP $ "res/imgs/" ++ filename
                    imageTexture <- SDL.createTextureFromSurface renderer imageSurface
                    return (name, (imageTexture, imageSurface))


quit :: GraphicsEnv -> IO ()
quit (GraphicsEnv _ window renderer graphicImages) = do
    destroyImages $ elems graphicImages
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    Font.quit
    SDL.quit
    where
        destroyImages :: [GraphicImage] -> IO ()
        destroyImages images = do
            mapM_ destroyImage images
            where destroyImage (imageTexture, imageSurface) = do
                    SDL.destroyTexture imageTexture
                    SDL.freeSurface imageSurface


output :: (MVar Integer, MVar Integer) -> GraphicsEnv -> RenderObject -> IO ()
output (fpsCounter, fpsLastTicks) env obj = SDL.clear renderer >> render env obj >> SDL.present renderer >> measureFPS
    where
        renderer = gRenderer env
        measureFPS = do
            ticks <- SDL.ticks
            lastTicks <- readMVar fpsLastTicks
            modifyMVar_ fpsCounter (return . succ)
            when ((lastTicks + 1000) < (fromIntegral ticks)) $ do
                swapMVar fpsLastTicks $ fromIntegral ticks
                curFps <- swapMVar fpsCounter (0::Integer)
                putStrLn $ "FPS: " ++ show curFps
