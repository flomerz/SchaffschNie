module Game.Output.Renderer (render) where

import Data.StateVar (($=))
import Data.Colour.SRGB (toSRGB24, RGB(..))
import Linear (V2(..), V4(..))
import Linear.Affine (Point(..))

import qualified SDL

import Game.Output.Types
import Game.Output.Shapes


render :: GraphicsEnv -> RenderObject -> IO ()
render env@((_, winY), _, renderer, graphicImages) obj = setRenderAttrs >> renderShape
    where
        setRenderAttrs = do
                let (RGB r g b) = toSRGB24 $ objColour obj
                SDL.rendererDrawColor renderer $= V4 r g b maxBound

        renderShape = case obj of
            RenderObject (Multiple objects) _ _ -> mapM_ (render env) objects
            RenderObject (Single shape) pos _ -> do
                case shape of
                    Rectangle size -> do
                        SDL.fillRect renderer $ createRectangle $ reflect (pos, size)

                    Image imgType size stripe -> do
                        let imageTexture = case imgType of
                                PlayerImage     -> fst $ imagePlayer graphicImages
                                BoxImage        -> fst $ imageBox graphicImages
                                LavaImage       -> fst $ imageLava graphicImages
                                _               -> fst $ imageAir graphicImages
                        let stripeRectange = case stripe of
                                Just stripeData -> createRectangle stripeData
                                _               -> Nothing
                        SDL.copy renderer imageTexture stripeRectange (createRectangle $ reflect (pos, size))

                where
                    createRectangle ((px, py), (sx, sy)) = Just $ SDL.Rectangle (P $ mkv2 px py) $ mkv2 sx sy

                    reflect :: ((Double, Double), (Double, Double)) -> ((Double, Double), (Double, Double))
                    reflect ((px, py), (sx, sy)) = ((px, ((fromIntegral winY) - py - sy)), (sx, sy))

                    mkv2 :: Enum a => Double -> Double -> V2 a
                    mkv2 x y = (V2 (toEnum $ floor x) (toEnum $ floor y))
