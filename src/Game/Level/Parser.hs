module Game.Level.Parser
    ( parseLevel
    , ObjectSignColumn
    ) where

import Game.Types


type ObjectSign = (Char, Int)
type ObjectSignColumn = ([ObjectSign],Int)


parseLevel :: [ObjectSignColumn] -> GameLevel
parseLevel lvl = map parseSignColumn lvl
        where parseSignColumn (signs, posX) = parseGameObjectColumn signs posX

parseGameObject :: Char -> Double -> GameObject
parseGameObject '.' = setPositionY $ initGameObjectAir
parseGameObject '#' = setPositionY $ initGameObjectBox
parseGameObject '_' = setPositionY $ initGameObjectLava
parseGameObject c = error $ "GameObject for char " ++ show c ++ " not found!"

parseGameObjects :: [ObjectSign] -> [GameObject]
parseGameObjects signs = map parseObjectSign signs
        where parseObjectSign (sign, posY) = parseGameObject sign (fromIntegral posY)

parseGameObjectColumn :: [ObjectSign] -> Int -> GameObjectColumn
parseGameObjectColumn signs posX = GameObjectColumn { oPositionX    = fromIntegral posX
                                                    , oObjects      = parseGameObjects signs
                                                    }
