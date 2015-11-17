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
    render gameObject = renderSimple
        where
            renderSimple = case gameObjectType of
                Air -> shape & colour_ (sRGB24 0x89 0xDE 0xFA)
                Box -> shape & colour_ (sRGB24 0xAB 0x67 0x09)
                Lava -> shape & colour_ (sRGB24 0xFF 0x00 0x00)
            renderImage = case gameObjectType of
                Air -> shape & colour_ (sRGB24 0x89 0xDE 0xFA)
                Box -> image "res/imgs/box.bmp"
                Lava -> image "res/imgs/lava.bmp"
            posY = renderScale * (oPositionY gameObject)
            gameObjectType = oType gameObject
            shape = rectangle_ (renderScale, renderScale) & posY_ posY
            image file = image_ file (renderScale, renderScale) Nothing & posY_ posY

instance GameRenderer GameObjectColumn where
    render (GameObjectColumn posX objs) = scene_ $ map renderObject objs
        where renderObject obj = render obj & posX_ (renderScale * posX)

instance GameRenderer GameData where
    render (GameData gameLevels (GameSession player curLvl curGamePosX)) = scene_ $ map renderColumn columns ++ [image_ "res/imgs/player.bmp" (renderScale, renderScale) (Just (255, 288)) & pos_ (scale $ pPosition player)]
        where
            renderColumn col@(GameObjectColumn posX _) = render $ col { oPositionX = posX - curGamePosX }
            columns = filter columnCondition $ gameLevels !! (curLvl - 1)
                where columnCondition column = and [ oPositionX column < curGamePosX + viewPort
                                                   , oPositionX column > curGamePosX - 1
                                                   ]

scale :: (Double, Double) -> (Double, Double)
scale (x, y) = (x * renderScale, y * renderScale)