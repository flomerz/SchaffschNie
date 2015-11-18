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
render env@(windowSize, _, renderer, graphicImages) obj = setRenderAttrs >> renderShape
    where
        setRenderAttrs = do
                let (RGB r g b) = toSRGB24 $ objColour obj
                SDL.rendererDrawColor renderer $= V4 r g b maxBound

        renderShape = case obj of
            RenderObject (Multiple objects) _ _ -> mapM_ (render env) objects
            RenderObject (Single shape) pos _ -> do
                let position = getPosition winY

                case shape of
                    Rectangle size -> do
                        SDL.fillRect renderer $ createRectangle position $ toupleF floor size

                    Image imgType size stripe -> do
                        let imageTexture = case imgType of
                                PlayerImage     -> fst $ imagePlayer graphicImages
                                BoxImage        -> fst $ imageBox graphicImages
                                LavaImage       -> fst $ imageLava graphicImages
                                _               -> fst $ imageAir graphicImages
                        let stripeRectange = case stripe of
                                Just (sx, sy)   -> Just $ SDL.Rectangle (P $ mkv2 0 0) $ mkv2 (floor sx) (floor sy)
                                _               -> Nothing
                        SDL.copy renderer imageTexture stripeRectange (createRectangle position $ toupleF floor size)

                where
                    createRectangle (px, py) (sx, sy) = Just $ SDL.Rectangle (P $ mkv2 px (py-sy)) $ mkv2 sx sy

                    getPosition winHeight = (\(x, y) -> (x, winHeight - y)) $ toupleF floor pos

                    winY = snd windowSize


mkv2 :: Enum a => Int -> Int -> V2 a
mkv2 x y = (V2 (toEnum x) (toEnum y))
