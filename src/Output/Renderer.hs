module Output.Renderer (render) where

import Prelude hiding (init)

import Control.Arrow
import Data.StateVar (($=))
import Data.Colour.SRGB (toSRGB24, RGB(..))
import Linear (V2(..), V4(..))
import Linear.Affine (Point(..))

import qualified SDL

import Output.Types
import Output.Shapes


import Debug.Trace


render :: GraphicsEnv -> RenderObject -> IO ()
render env@(window, renderer) obj = setRenderAttrs >> renderShape
    where
        setRenderAttrs = do
                let (RGB r g b) = toSRGB24 $ objColour obj
                SDL.rendererDrawColor renderer $= V4 r g b maxBound

        renderShape = case obj of
            RenderObject (Multiple objects) _ _ -> SDL.clear renderer >> mapM_ (render env) objects
            RenderObject (Single shape) pos _ -> do
                winY <- getWindowHeight

                let (px, py) = objPos >>> (\(x, y) -> (floor x, winY - floor y)) $ obj

                case shape of
                    Rectangle size -> do
                        let (sx, sy) = (\(x,y) -> (floor x, floor $ -y)) size
                        trace ("x:" ++ show sx ++ " y:" ++ show sy) return ()
                        let rectangle = Just . SDL.Rectangle (P $ mkv2 px py) $ mkv2 sx sy
                        SDL.fillRect renderer rectangle

        getWindowHeight = do
                winConf <- SDL.getWindowConfig window
                return $ (\(V2 _ y) -> fromIntegral y) $ SDL.windowInitialSize winConf


mkv2 :: Enum a => Int -> Int -> V2 a
mkv2 x y = (V2 (toEnum x) (toEnum y))
