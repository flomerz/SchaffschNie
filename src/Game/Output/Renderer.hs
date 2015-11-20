module Game.Output.Renderer (render) where

import Data.Map
import Linear (V2(..))
import Linear.Affine (Point(..))

import qualified SDL
import qualified SDL.Raw.Types as SDL (Color(..))
import qualified Graphics.UI.SDL.TTF as Font

import Game.Util
import Game.Output.Types
import Game.Output.Shapes
import Game.Output.Util


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

            Text txt fontSize -> do
                  font <- Font.openFont "res/fonts/Boxy-Bold.ttf" fontSize
                  textSize <- Font.sizeText font txt
                  textSurface <- fmap unmanagedSurface $ Font.renderTextSolid font txt (SDL.Color 0 0x0D 0xBF 0)
                  textTexture <- SDL.createTextureFromSurface renderer textSurface
                  SDL.copy renderer textTexture Nothing $ createRectangle $ reflect (pos, toupleF fromIntegral textSize)
                  SDL.destroyTexture textTexture
                  SDL.freeSurface textSurface
                  Font.closeFont font

        where
            createRectangle ((px, py), (sx, sy)) = Just $ SDL.Rectangle (P $ mkv2 px py) $ mkv2 sx sy

            reflect :: ((Double, Double), (Double, Double)) -> ((Double, Double), (Double, Double))
            reflect ((px, py), (sx, sy)) = ((px, ((fromIntegral winY) - py - sy)), (sx, sy))

            mkv2 :: Enum a => Double -> Double -> V2 a
            mkv2 x y = (V2 (toEnum $ floor x) (toEnum $ floor y))
