module Game.Types where

import Data.Default


-- TYPE DEFINITIONS
data GameObjectType = Air
                    | Box
                    | Lava
                    deriving (Show, Eq)

data GameObject = GameObject { oPositionY               :: Int
                             , oType                    :: GameObjectType
                             , oColliding               :: Bool
                             , oDrivable                :: Bool
                             , oAccelerationY           :: Double
                             } deriving (Show, Eq)

data GameObjectColumn = GameObjectColumn { oPositionX   :: Int
                                         , oObjects     :: [GameObject]
                                         } deriving (Show, Eq)

data GameLevel = GameLevel { wObjectColumns     :: [GameObjectColumn]
                           } deriving (Show, Eq)

data GamePlayer = GamePlayer { pPosition :: (Double, Double)
                             } deriving (Show, Eq)


-- TYPE INITIALIZERS
instance Default GameObject where
    def = GameObject { oPositionY           = 0
                     , oType                = error "Object Type wasn't define"
                     , oColliding           = False
                     , oDrivable            = False
                     , oAccelerationY       = 0
                     }

initGamePlayer :: GamePlayer
initGamePlayer = GamePlayer (0,1)

initGameObjectAir :: GameObject
initGameObjectAir = def { oType             = Air
                        , oAccelerationY    = 9.81
                        }

initGameObjectBox :: GameObject
initGameObjectBox = def { oType             = Box
                        , oColliding        = True
                        , oDrivable         = True
                        }

initGameObjectLava :: GameObject
initGameObjectLava = def { oType            = Lava
                         , oColliding       = True
                         }


-- TYPE ATTRIBUT CHANGE FUNCTIONS
setPositionY :: GameObject -> Int -> GameObject
setPositionY obj pos = obj { oPositionY = pos }
