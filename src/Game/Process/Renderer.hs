module Game.Process.Renderer where

import Game.Types
import Game.Output.Shapes


renderScale :: Double
renderScale = 10

viewPort :: Double
viewPort = 25

class GameRenderer a where
    render :: a -> RenderObject

instance GameRenderer GameObject where
    render gameObject = case gameObjectType of
        Air -> shape & colour_ (sRGB24 0x00 0x00 0xFF)
        Box -> shape & colour_ (sRGB24 0x00 0xFF 0x00)
        Lava -> shape & colour_ (sRGB24 0xFF 0x00 0x00)
        where
            posY = renderScale * (fromIntegral $ oPositionY gameObject)
            gameObjectType = oType gameObject
            shape = rectangle_ (renderScale, renderScale) & posY_ posY

instance GameRenderer GameObjectColumn where
    render (GameObjectColumn posX objs) = scene_ $ map renderObject objs
        where renderObject obj = render obj & posX_ (renderScale * (fromIntegral posX))

instance GameRenderer GameLevel where
    render (GameLevel objColumns) = scene_ $ map render objColumns

instance GameRenderer GameData where
    render (GameData gameLevels gameSession) = render $ currentLevelView
        where
            GameLevel currentLevelColumns = gameLevels !! (fromIntegral (gLevel gameSession) - 1)
            currentLevelView = GameLevel $ filter (\column -> and [fromIntegral (oPositionX column) < currentPositionX + viewPort
                                                                    , fromIntegral (oPositionX column) > currentPositionX]) currentLevelColumns
            currentPositionX = gPosX gameSession