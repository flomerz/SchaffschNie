module Game.Types where

import Data.Default


-- TYPE DEFINITIONS
data GameObjectType = GameImage String
                    | GameAnimation String Int Double
                    deriving (Show, Eq)

data GameObject = GameObject { oPositionY               :: Double
                             , oType                    :: Maybe GameObjectType
                             , oColliding               :: Bool
                             , oDrivable                :: Bool
                             , oAccelerationY           :: Double
                             } deriving (Show, Eq)

data GameObjectColumn = GameObjectColumn { oPositionX   :: Double
                                         , oObjects     :: [GameObject]
                                         } deriving (Show, Eq)

type GameLevel = [GameObjectColumn]

data GamePlayer = GamePlayer { pPosX :: Double
                             , pPosY :: Double
                             , pV    :: Double
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
initGamePlayer = GamePlayer 2 15 0

initGameSession :: GameSession
initGameSession = GameSession initGamePlayer 1 0 0

initGameObjectFallable :: Maybe GameObjectType -> Double -> GameObject
initGameObjectFallable objType objAccelerationY = def { oType             = objType
                                                      , oAccelerationY    = objAccelerationY
                                                      }

initGameObjectDrivable :: Maybe GameObjectType -> GameObject
initGameObjectDrivable objType = def { oType          = objType
                                     , oColliding     = True
                                     , oDrivable      = True
                                     }

initGameObjectObstacle :: Maybe GameObjectType -> GameObject
initGameObjectObstacle objType = def { oType            = objType
                                     , oColliding       = True
                                     }


-- TYPE ATTRIBUT CHANGE FUNCTIONS
setPositionY :: GameObject -> Double -> GameObject
setPositionY obj pos = obj { oPositionY = pos }
