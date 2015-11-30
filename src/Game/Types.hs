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

data GamePlayer = GamePlayer { pPosX          :: Double
                             , pPosY          :: Double
                             , pV             :: Double
                             , pAccelerationY :: Double
                             } deriving (Show, Eq)

data GameSession = GameSession { gPlayer    :: GamePlayer
                               , gLevel     :: Int
                               , gPosX      :: Double
                               , gTries     :: Int
                               } deriving (Show)

type RenderScale = Double
type WindowSize = (Int, Int)
type ResolutionSettings = (WindowSize, RenderScale)

data GameSettings = GameSettings { gWindowSize   :: WindowSize
                                 , gRenderScale  :: RenderScale
                                 , gWorldSpeed   :: Double
                                 , gJumpSpeed    :: Double
                                 } deriving (Show)

data GameData = GameData { gLevels      :: [GameLevel]
                         , gSession     :: GameSession
                         , gSettings    :: GameSettings
                         } deriving (Show)


-- TYPE INITIALIZERS
instance Default GameObject where
    def = GameObject { oPositionY           = 0
                     , oType                = error "Object Type wasn't define"
                     , oColliding           = False
                     , oDrivable            = False
                     , oAccelerationY       = 0
                     }


initGameData :: GameSession -> GameSettings -> [GameLevel] -> GameData
initGameData gameSession gameSettings lvls = GameData lvls gameSession gameSettings

initGamePlayer :: Double -> Double -> Double -> GamePlayer
initGamePlayer x y acc = GamePlayer x y 0 acc

initGameSession :: GamePlayer -> Int -> GameSession
initGameSession gamePlayer level = GameSession gamePlayer level 0 0

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
setGameObjectPositionY :: GameObject -> Double -> GameObject
setGameObjectPositionY obj pos = obj { oPositionY = pos }

setGameSession_ :: GameData -> GameSession -> GameData
setGameSession_ gameData gameSession = gameData { gSession = gameSession }

setGamePlayer_ :: GameData -> GamePlayer -> GameData
setGamePlayer_ gameData gamePlayer = gameData { gSession = (gSession gameData) { gPlayer = gamePlayer } }


currentGameLevel :: GameData -> GameLevel
currentGameLevel gameData = gameLevels !! currentLevel
    where
        gameLevels = gLevels gameData
        currentLevel = (gLevel $ gSession gameData) - 1