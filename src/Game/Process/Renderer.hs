module Game.Process.Renderer where

import Game.AppTypes (ResolutionSettings)
import Game.Types
import Game.Output.Shapes

import Game.Util


class GameRenderer a where
    render :: ResolutionSettings -> a -> RenderObject

instance GameRenderer GameObject where
    render (_, renderScale) gameObject = shape
        where
            shape = case (oType gameObject) of
                Air -> rect & colour_ (sRGB24 0x89 0xDE 0xFA)
                Box -> image "res/imgs/box.bmp"
                Lava -> image "res/imgs/lava.bmp"
            posY = renderScale * (oPositionY gameObject)
            image file = image_ file (renderScale, renderScale) Nothing & posY_ posY
            rect = rectangle_ (renderScale, renderScale) & posY_ posY

instance GameRenderer GameObjectColumn where
    render resSettings@(_, renderScale) (GameObjectColumn posX objs) = scene_ $ map renderObject objs
        where renderObject obj = render resSettings obj & posX_ (renderScale * posX)

instance GameRenderer GameData where
    render resSettings@(windowSize, renderScale) (GameData gameLevels (GameSession player curLvl curGamePosX)) = scene_ $ levelShape ++ [playerShape]
        where
            playerShape = image_ "res/imgs/player.bmp" (renderScale, renderScale) (Just (255, 288)) & pos_ (toupleF (* renderScale) $ pPosition player)
            levelShape = map renderColumn columns
            renderColumn col@(GameObjectColumn posX _) = render resSettings $ col { oPositionX = posX - curGamePosX }
            columns = filter columnCondition $ gameLevels !! (curLvl - 1)
                where
                    columnCondition column = and [ oPositionX column < curGamePosX + viewPort
                                                 , oPositionX column > curGamePosX - 1
                                                 ]
                    viewPort = (fromIntegral $ fst windowSize) / renderScale
