module Game.Process.Renderer where

import Game.Types
import Game.Output.Shapes


renderScale :: Double
renderScale = 32

viewPort :: Double
viewPort = 40

class GameRenderer a where
    render :: a -> RenderObject

instance GameRenderer GameObject where
    render gameObject = case gameObjectType of
        Air -> shape & colour_ (sRGB24 0x89 0xDE 0xFA)
        Box -> image "res/imgs/box.bmp"
        Lava -> image "res/imgs/lava.bmp"
        where
            posY = renderScale * (oPositionY gameObject)
            gameObjectType = oType gameObject
            shape = rectangle_ (renderScale, renderScale) & posY_ posY
            image file = image_ file (renderScale, renderScale) & posY_ posY

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