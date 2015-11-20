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
                Box -> image "box"
                Lava -> image "lava"
                _ -> error "render object type not supported"
            posY = renderScale * (oPositionY gameObject)
            image imgType = image_ imgType (renderScale, renderScale) Nothing & posY_ posY


instance GameRenderer GameObjectColumn where
    render resSettings@(_, renderScale) (GameObjectColumn posX objs) = scene_ $ map renderObject $ filter objectCondition objs
        where
            renderObject obj = render resSettings obj & posX_ (renderScale * posX)
            objectCondition obj = oType obj /= Air


instance GameRenderer GameData where
    render resSettings@(windowSize, renderScale) (GameData gameLevels (GameSession player curLvl curGamePosX)) = scene_ $ backgroundShape ++ levelShape ++ playerShape
        where
            backgroundShape = [image_ "background" (toupleF fromIntegral windowSize) Nothing]
            levelShape = map renderColumn columns
            playerShape = [image_ "player" (renderScale, renderScale) (Just ((0, 0), (255, 288))) & pos_ (toupleF (* renderScale) $ pPosition player)]
            renderColumn col@(GameObjectColumn posX _) = render resSettings $ col { oPositionX = posX - curGamePosX }
            columns = filter columnCondition $ gameLevels !! (curLvl - 1)
                where
                    columnCondition column = and [ oPositionX column < curGamePosX + viewPort
                                                 , oPositionX column > curGamePosX - 1
                                                 ]
                    viewPort = (fromIntegral $ fst windowSize) / renderScale
