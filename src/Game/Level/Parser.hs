module Game.Level.Parser
    ( parseLevel
    , ObjectSignColumn
    ) where

import Game.Types


type ObjectSign = (Char, Int)
type ObjectSignColumn = ([ObjectSign],Int)


parseLevel :: [ObjectSignColumn] -> GameLevel
parseLevel lvl = GameLevel { wObjectColumns = map parseSignColumn lvl }
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
