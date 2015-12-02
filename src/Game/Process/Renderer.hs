module Game.Process.Renderer where

import Data.Maybe

import Game.Types
import Game.Output.Shapes

import Game.Util


class GameRenderer a where
    render :: ResolutionSettings -> (Double, a) -> RenderObject

instance GameRenderer GameObject where
    render (_, renderScale) (time, gameObject) = shape
        where
            shape = case (oType gameObject) of
                Just (GameImage imgStr) -> image imgStr
                Just (GameAnimation dirStr imgCount imgSpeed) -> image $ sprite time dirStr imgCount imgSpeed
                _ -> error "render object type not supported"
            posY = renderScale * (oPositionY gameObject)
            image imgType = image_ imgType (renderScale, renderScale) Nothing & posY_ posY


instance GameRenderer GameObjectColumn where
    render resSettings@(_, renderScale) (time, (GameObjectColumn posX objs)) = scene_ $ map renderObject $ filter objectCondition objs
        where
            renderObject obj = render resSettings (time, obj) & posX_ (renderScale * posX)
            objectCondition obj = isJust $ oType obj


instance GameRenderer GameData where
    render resSettings@(windowSize, renderScale) (time, gameData@(GameData _ (GameSession player curLvl curGamePosX curTries progress progressBest sessionDone) _)) = renderObject
        where
            renderObject = scene_ $ backgroundShape ++ levelShape ++ playerShape ++ (if sessionDone then sessionDoneText else [])
            backgroundShape = [image_ "background" dWindowSize Nothing]
            levelShape = map renderColumn columns ++ levelText
                where
                    levelText = [ text_ ("Level: " ++ show curLvl) 40 & pos_ (180 , y - 60)
                                , text_ ("Tries: " ++ show curTries) 40 & pos_ (480, y - 60)
                                , text_ ("Best: " ++ show progressBest ++ "%") 40 & pos_ (800, y - 60)
                                , text_ ("Now:  " ++ show progress ++ "%") 40 & pos_ (800, y - 110)
                                ]
            playerShape = [image_ playerImage (renderScale, renderScale) Nothing & pos_ (toupleF (* renderScale) $ playerPos)]
                where
                    playerPos = (pPosX player, pPosY player)
                    playerImage | pV player == 0    = sprite time "player/run/" 3 10
                                | otherwise         = "player/jump"
            sessionDoneText = [ text_ ("Level Done!") 100 & pos_ (250, y / 2)
                              , text_ ("Press space to restart the level!") 40 & pos_ (200, (y / 2) - 60)
                              ]

            renderColumn col@(GameObjectColumn posX _) = render resSettings $ (time, col { oPositionX = posX - curGamePosX })
            columns = filter columnCondition $ currentGameLevel gameData
                where
                    columnCondition column = and [ oPositionX column < curGamePosX + viewPort
                                                 , oPositionX column > curGamePosX - 1
                                                 ]
                    viewPort = x / renderScale
            dWindowSize@(x, y) = toupleF fromIntegral windowSize


sprite :: Double -> String -> Int -> Double -> String
sprite time dir imgCount imgsPerSec = dir ++ show spriteNumber
    where spriteNumber  = (floor (imgsPerSec * time) `mod` imgCount) + 1