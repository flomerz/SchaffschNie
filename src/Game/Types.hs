module Game.Types where

import Data.Default


-- TYPE DEFINITIONS
data GameObjectType = Air
                    | Box
                    | Lava
                    deriving (Show, Eq)

data GameObject = GameObject { oPositionY               :: Double
                             , oType                    :: GameObjectType
                             , oColliding               :: Bool
                             , oDrivable                :: Bool
                             , oAccelerationY           :: Double
                             } deriving (Show, Eq)

data GameObjectColumn = GameObjectColumn { oPositionX   :: Double
                                         , oObjects     :: [GameObject]
                                         } deriving (Show, Eq)

type GameLevel = [GameObjectColumn]

data GamePlayer = GamePlayer { pPosition :: (Double, Double)
                             } deriving (Show, Eq)

data GameSession = GameSession { gPlayer    :: GamePlayer
                               , gLevel     :: Int
                               , gPosX      :: Double
                               , gTries     :: Int
                               }

data GameData = GameData { gLevels      :: [GameLevel]
                         , gSession     :: GameSession
                         }


currentGameLevel :: GameData -> GameLevel
currentGameLevel gameData = gameLevels !! currentLevel
    where
        gameLevels = gLevels gameData
        currentLevel = (gLevel $ gSession gameData) - 1


-- TYPE INITIALIZERS
instance Default GameObject where
    def = GameObject { oPositionY           = 0
                     , oType                = error "Object Type wasn't define"
                     , oColliding           = False
                     , oDrivable            = False
                     , oAccelerationY       = 0
                     }

initGameData :: [GameLevel] -> GameData
initGameData lvls = GameData lvls initGameSession

initGamePlayer :: GamePlayer
initGamePlayer = GamePlayer (3,1)

initGameSession :: GameSession
initGameSession = GameSession initGamePlayer 1 0 0

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
setPositionY :: GameObject -> Double -> GameObject
setPositionY obj pos = obj { oPositionY = pos }
