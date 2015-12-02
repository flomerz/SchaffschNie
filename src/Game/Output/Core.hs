module Game.Output.Core
    ( init
    , quit
    , output
    , module Game.Output.Types
    , module Game.Output.Shapes
    ) where

import Prelude hiding (init)

import Data.Map (Map, fromList, elems)
import Control.Monad
import Control.Concurrent
import System.Process
import Data.Text (pack)
import Data.StateVar (($=))
import Linear (V2(..), V4(..))

import qualified SDL
import qualified Graphics.UI.SDL.TTF as Font

import Game.Util
import Game.AppTypes
import Game.Output.Types
import Game.Output.Shapes
import qualified Game.Output.Renderer as Graphics
import qualified Game.Output.Audio as Audio


init :: WindowSize -> String -> IO GraphicsEnv
init winSize@(winWidth, winHeight) title = do
    SDL.initialize [SDL.InitVideo]
    _ <- Font.init

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
            loadImageDir "res/imgs/"
            where
                loadImageDir :: FilePath -> IO (Map String GraphicImage)
                loadImageDir filePath = do
                    imageFiles <- getRecursiveContents filePath
                    images <- mapM (loadImage filePath) imageFiles
                    return $ fromList images
                loadImage :: FilePath -> String -> IO (String, GraphicImage)
                loadImage filePath fileName = do
                    let name = takeWhile (/='.') fileName
                    imageSurface <- SDL.loadBMP $ filePath ++ fileName
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


output :: (MVar Integer, MVar Integer, MVar ProcessHandle) -> GraphicsEnv -> AppOutput -> IO ()
output (fpsCounter, fpsLastTicks, audioProcessVar) environment appOutput = do
    -- grafic rendering
    SDL.clear renderer >> Graphics.render environment obj >> SDL.present renderer >> measureFPS

    -- sound
    case outAudio appOutput of
        Just sound -> Audio.playSound sound audioProcessVar
        _ -> return ()

    where
        obj = outRenderObject appOutput
        renderer = gRenderer environment
        measureFPS = do
            ticks <- SDL.ticks
            lastTicks <- readMVar fpsLastTicks
            modifyMVar_ fpsCounter (return . succ)
            when ((lastTicks + 1000) < (fromIntegral ticks)) $ do
                _ <- swapMVar fpsLastTicks $ fromIntegral ticks
                curFps <- swapMVar fpsCounter (0::Integer)
                putStrLn $ "FPS: " ++ show curFps
