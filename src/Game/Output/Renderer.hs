module Game.Output.Renderer (render) where

import Data.StateVar (($=))
import Data.Colour.SRGB (toSRGB24, RGB(..))
import Linear (V2(..), V4(..))
import Linear.Affine (Point(..))

import qualified SDL

import Game.Util
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
                        SDL.fillRect renderer $ createRectangle position $ toupleF floor size

                    ImageRectangle file size stripe -> do
                        imageSurface <- SDL.loadBMP file
                        imageTexture <- SDL.createTextureFromSurface renderer imageSurface
                        let stripeRectange = case stripe of
                                Just (sx, sy) -> Just $ SDL.Rectangle (P $ mkv2 0 0) $ mkv2 (floor sx) (floor sy)
                                _ -> Nothing
                        SDL.copy renderer imageTexture stripeRectange (createRectangle position $ toupleF floor size)

                where
                    createRectangle (px, py) (sx, sy) = Just $ SDL.Rectangle (P $ mkv2 px (py-sy)) $ mkv2 sx sy

                    getPosition winHeight = (\(x, y) -> (x, winHeight - y)) $ toupleF floor pos

                    getWindowHeight = do
                        winConf <- SDL.getWindowConfig window
                        return $ (\(V2 _ y) -> fromIntegral y) $ SDL.windowInitialSize winConf


mkv2 :: Enum a => Int -> Int -> V2 a
mkv2 x y = (V2 (toEnum x) (toEnum y))
