module Level.GameParser 
    ( parse
    , ObjectSignColumn
    ) where

import GameTypes


type ObjectSign = (Char, Int)
type ObjectSignColumn = ([ObjectSign],Int)


parse :: [ObjectSignColumn] -> GameWorld
parse world = GameWorld { wPlayer          = initPlayer
                        , wObjectColumns   = map parseSignColumn world
                        }
        where parseSignColumn (signs, posX) = parseGameObjectColumn signs posX


parseGameObject :: Char -> Int -> GameObject
parseGameObject '.' = setPositionY $ initGameObjectAir
parseGameObject '#' = setPositionY $ initGameObjectBox
parseGameObject '_' = setPositionY $ initGameObjectLava
parseGameObject c = error $ "GameObject for char " ++ show c ++ " not found!"

parseGameObjects :: [ObjectSign] -> [GameObject]
parseGameObjects signs = map parseObjectSign signs
        where parseObjectSign (sign, posY) = parseGameObject sign posY

parseGameObjectColumn :: [ObjectSign] -> Int -> GameObjectColumn
parseGameObjectColumn signs posX = GameObjectColumn { oPositionX    = posX
                                                    , oObjects      = parseGameObjects signs
                                                    }
