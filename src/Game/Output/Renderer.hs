module Game.Output.Renderer (render) where

import Data.Map
import Linear (V2(..))
import Linear.Affine (Point(..))

import qualified SDL

import Game.Output.Types
import Game.Output.Shapes


render :: GraphicsEnv -> RenderObject -> IO ()
render env@(GraphicsEnv (_, winY) _ renderer graphicImages) obj = case obj of
    RenderObject (Multiple objects) _ -> mapM_ (render env) objects
    RenderObject (Single shape) pos -> do
        case shape of
            Image imgName imgSize stripe -> do
                let imageTexture = fst $ graphicImages ! imgName
                let stripeRectange = case stripe of
                        Just stripeData -> createRectangle stripeData
                        _               -> Nothing
                SDL.copy renderer imageTexture stripeRectange (createRectangle $ reflect (pos, imgSize))

        where
            createRectangle ((px, py), (sx, sy)) = Just $ SDL.Rectangle (P $ mkv2 px py) $ mkv2 sx sy

            reflect :: ((Double, Double), (Double, Double)) -> ((Double, Double), (Double, Double))
            reflect ((px, py), (sx, sy)) = ((px, ((fromIntegral winY) - py - sy)), (sx, sy))

            mkv2 :: Enum a => Double -> Double -> V2 a
            mkv2 x y = (V2 (toEnum $ floor x) (toEnum $ floor y))
