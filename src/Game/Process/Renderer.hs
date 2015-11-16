module Game.Process.Renderer where

import Game.Types
import Game.Output.Shapes


renderScale :: Double
renderScale = 10

viewPort :: Double
viewPort = 30

class GameRenderer a where
    render :: a -> RenderObject

instance GameRenderer GameObject where
    render gameObject = case gameObjectType of
        Air -> shape & colour_ (sRGB24 0x00 0x00 0xFF)
        Box -> shape & colour_ (sRGB24 0x00 0xFF 0x00)
        Lava -> shape & colour_ (sRGB24 0xFF 0x00 0x00)
        where
            posY = renderScale * (oPositionY gameObject)
            gameObjectType = oType gameObject
            shape = rectangle_ (renderScale, renderScale) & posY_ posY

instance GameRenderer GameObjectColumn where
    render (GameObjectColumn posX objs) = scene_ $ map renderObject objs
        where renderObject obj = render obj & posX_ (renderScale * posX)

instance GameRenderer GameData where
    render (GameData gameLevels (GameSession _ curLvl curGamePosX)) = scene_ $ map renderColumn columns
        where
            renderColumn col@(GameObjectColumn posX _) = render $ col { oPositionX = posX - curGamePosX }
            columns = filter columnCondition $ gameLevels !! (curLvl - 1)
                where columnCondition column = and [ oPositionX column < curGamePosX + viewPort
                                                   , oPositionX column > curGamePosX - 1
                                                   ]