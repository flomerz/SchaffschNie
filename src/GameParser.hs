module GameParser 
    ( parseGameWorld
    , ObjectSignColumn
    ) where

import GameTypes


type ObjectSign = (Char, Int)
type ObjectSignColumn = ([ObjectSign],Int)

parseGameObject :: Char -> Int -> GameObject
parseGameObject '.' = setPositionY $ initGameObjectAir
parseGameObject '#' = setPositionY $ initGameObjectBox
parseGameObject '_' = setPositionY $ initGameObjectLava
parseGameObject c = error $ "GameObject for char " ++ show c ++ " not found!"

parseGameObjects :: [ObjectSign] -> [GameObject]
parseGameObjects signs = map parse signs
        where parse (sign, posY) = parseGameObject sign posY

parseGameObjectColumn :: [ObjectSign] -> Int -> GameObjectColumn
parseGameObjectColumn signs posX = GameObjectColumn { oPositionX    = posX
                                                    , oObjects      = parseGameObjects signs
                                                    }
parseGameWorld :: [ObjectSignColumn] -> GameWorld
parseGameWorld world = GameWorld { wPlayer          = initPlayer
                                 , wObjectColumns   = map parse world
                                 }
        where parse (signs, posX) = parseGameObjectColumn signs posX
