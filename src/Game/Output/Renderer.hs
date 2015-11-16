module Game.Output.Renderer (render) where

import Data.StateVar (($=))
import Data.Colour.SRGB (toSRGB24, RGB(..))
import Linear (V2(..), V4(..))
import Linear.Affine (Point(..))

import qualified SDL

import Game.Output.Types
import Game.Output.Shapes


render :: GraphicsEnv -> RenderObject -> IO ()
render env@(window, renderer) obj = setRenderAttrs >> renderShape
    where
        setRenderAttrs = do
                let (RGB r g b) = toSRGB24 $ objColour obj
                SDL.rendererDrawColor renderer $= V4 r g b maxBound

        renderShape = case obj of
            RenderObject (Multiple objects) _ _ -> mapM_ (render env) objects
            RenderObject (Single shape) pos _ -> do
                (px, py) <- getPosition <$> getWindowHeight

                case shape of
                    Rectangle size -> do
                        let (sx, sy) = (\(x,y) -> (floor x, floor $ -y)) size
                        let rectangle = Just . SDL.Rectangle (P $ mkv2 px py) $ mkv2 sx sy
                        SDL.fillRect renderer rectangle
                    ImageRectangle file size -> do
                        let (sx, sy) = (\(x,y) -> (floor x, floor y)) size
                        let rectangle = Just . SDL.Rectangle (P $ mkv2 px (py-sy)) $ mkv2 sx sy
                        imageSurface <- SDL.loadBMP file
                        imageTexture <- SDL.createTextureFromSurface renderer imageSurface
                        SDL.copy renderer imageTexture Nothing rectangle

                where
                    getPosition winHeight = (\(x, y) -> (floor x, winHeight - floor y)) pos

                    getWindowHeight = do
                        winConf <- SDL.getWindowConfig window
                        return $ (\(V2 _ y) -> fromIntegral y) $ SDL.windowInitialSize winConf


mkv2 :: Enum a => Int -> Int -> V2 a
mkv2 x y = (V2 (toEnum x) (toEnum y))
