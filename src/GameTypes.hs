module GameTypes where

import Data.Default


-- TYPE DEFINITIONS
data GameObjectType = Air
                    | Box
                    | Lava
                    deriving (Show, Eq)

data GamePlayer = Player { pPosition :: (Double, Double)
                         } deriving (Show, Eq)

data GameObject = GameObject { oPositionY :: Int
                             , oType :: GameObjectType
                             , oCollideTop :: Bool
                             , oCollideFront :: Bool
                             , oFallingAcceleration :: Double
                             } deriving (Show, Eq)

data GameObjectColumn = GameObjectColumn { oPositionX :: Int
                                         , oObjects :: [GameObject]
                                         } deriving (Show, Eq)

data GameWorld = GameWorld { wPlayer :: GamePlayer
                           , wObjectColumns :: [GameObjectColumn]
                           } deriving (Show, Eq)


-- TYPE INITIALIZERS
instance Default GameObject where
    def = GameObject { oPositionY = 0
                     , oType = error "Object Type wasn't define"
                     , oCollideTop = False
                     , oCollideFront = False
                     , oFallingAcceleration = 0
                     }

initPlayer :: GamePlayer
initPlayer = Player (0,1)

initGameObjectAir :: GameObject
initGameObjectAir = def { oType = Air
                        , oFallingAcceleration = 9.81
                        }

initGameObjectBox :: GameObject
initGameObjectBox = def { oType = Box 
                        , oCollideFront = True
                        }

initGameObjectLava :: GameObject
initGameObjectLava = def { oType = Lava
                         , oCollideTop = True
                         , oCollideFront = True
                         }


-- TYPE ATTRIBUT CHANGE FUNCTIONS
setGameObjectPosition :: GameObject -> Int -> GameObject
setGameObjectPosition obj pos = obj { oPositionY = pos }
