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
                position <- getPosition <$> getWindowHeight

                case shape of
                    Rectangle size -> do
                        SDL.fillRect renderer $ Just $ createRectangle position $ floorT size

                    ImageRectangle file size -> do
                        imageSurface <- SDL.loadBMP file
                        imageTexture <- SDL.createTextureFromSurface renderer imageSurface
                        SDL.copy renderer imageTexture Nothing $ Just $ createRectangle position $ floorT size

                where
                    createRectangle (px, py) (sx, sy) = SDL.Rectangle (P $ mkv2 px (py-sy)) $ mkv2 sx sy

                    getPosition winHeight = (\(x, y) -> (x, winHeight - y)) $ floorT pos

                    getWindowHeight = do
                        winConf <- SDL.getWindowConfig window
                        return $ (\(V2 _ y) -> fromIntegral y) $ SDL.windowInitialSize winConf


floorT :: (Double, Double) -> (Int, Int)
floorT (a1, a2) = (floor a1, floor a2)


mkv2 :: Enum a => Int -> Int -> V2 a
mkv2 x y = (V2 (toEnum x) (toEnum y))
