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
parseGameObject '.' = setPositionY $ initGameObjectFallable Nothing 100
parseGameObject 'G' = setPositionY $ initGameObjectDrivable $ image_ "ground_1"
parseGameObject 'g' = setPositionY $ initGameObjectDrivable $ image_ "ground_2"
parseGameObject 'B' = setPositionY $ initGameObjectDrivable $ image_ "box_1"
parseGameObject 'L' = setPositionY $ initGameObjectObstacle $ animation_ "lava/" 45 10
parseGameObject c = error $ "GameObject for char " ++ show c ++ " not found!"

parseGameObjects :: [ObjectSign] -> [GameObject]
parseGameObjects signs = map parseObjectSign signs
        where parseObjectSign (sign, posY) = parseGameObject sign (fromIntegral posY)

parseGameObjectColumn :: [ObjectSign] -> Int -> GameObjectColumn
parseGameObjectColumn signs posX = GameObjectColumn { oPositionX    = fromIntegral posX
                                                    , oObjects      = parseGameObjects signs
                                                    }

image_ :: String -> Maybe GameObjectType
image_ name = Just $ GameImage name

animation_ :: String -> Int -> Double -> Maybe GameObjectType
animation_ imgDir imgCount imgSpeed = Just $ GameAnimation imgDir imgCount imgSpeed