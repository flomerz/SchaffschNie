 {-# LANGUAGE Arrows            #-}
module Game.Process.Logic where

import FRP.Yampa

import Game.AppTypes
import Game.Types
import Game.Process.Event
import Game.Input.Events


import Debug.Trace


-- Main Function
game :: GameData -> SF AppInput GameData
game gameData0 = proc _ -> do
    nextSession <- playerFalling gamePlayer gameLevel <<< moveWorld gameSession gameLevel -< ()
    let nextGameData = gameData0 { gSession = nextSession }
    returnA -< nextGameData
    where
        gameLevel = (currentGameLevel gameData0)
        gameSession = (gSession gameData0)
        gamePlayer = (gPlayer gameSession)



-- World SF
moveWorld :: GameSession -> GameLevel -> SF () GameSession
moveWorld gameSession0 gameLevel = switch sf (\_ -> moveWorld gameSession0 gameLevel)
    where
        sf = proc _ -> do
            nextPosX <- moveWorldPosition (gPosX gameSession0) -< ()
            endReached <- edge -< checkEndOfWorld nextPosX gameLevel
            let nextGameSession = gameSession0 { gPosX = nextPosX }
            returnA -< (nextGameSession, endReached)

moveWorldPosition :: Double -> SF () Double
moveWorldPosition gPosX0 = constant 5 >>> imIntegral gPosX0


-- World Functions
checkEndOfWorld :: Double -> GameLevel -> Bool
checkEndOfWorld gamePosX gameLevel = endReached
    where endReached = (getEndPositionOfLevel <= gamePosX)
          getEndPositionOfLevel = (oPositionX $ last gameLevel) - 5



-- Player SF State
playerDriving :: GamePlayer -> GameLevel -> SF GameSession GameSession
playerDriving gamePlayer0 gameLevel = trace ("drive " ++ show gamePlayer0) $ switch sf (\_ -> playerFalling gamePlayer0 gameLevel)
    where
        sf = proc gameSession -> do
            let nextGameSession = gameSession { gPlayer = gamePlayer0 }
            playerFallingEvent <- edge -< checkPlayerStartFalling gameLevel nextGameSession
            returnA -< (nextGameSession, playerFallingEvent)


playerFalling :: GamePlayer -> GameLevel -> SF GameSession GameSession
playerFalling gamePlayer0 gameLevel = trace ("fall " ++ show gamePlayer0) $ switch sf (\gamePlayer -> playerDriving gamePlayer gameLevel)
    where
        sf = proc gameSession -> do
            nextPlayer <- fallPlayer gamePlayer0 -< ()
            let nextGameSession = gameSession { gPlayer = nextPlayer }
            playerDrivingEvent <- edge -< checkPlayerStopFalling gameLevel nextGameSession
            returnA -< (nextGameSession, playerDrivingEvent `tag` ajustPlayer nextPlayer)
            where ajustPlayer player = player { pPosY = (fromInteger . ceiling $ (pPosY player)) , pV = 0}


-- Player SF
fallPlayer :: GamePlayer -> SF () GamePlayer
fallPlayer gamePlayer0 = proc _ -> do
    nextV <- imIntegral playerV0 -< -20
    nextY <- imIntegral playerPosY0 -< nextV
    returnA -< gamePlayer0 { pV = nextV
                           , pPosY = nextY
                           }
    where
        playerV0 = pV gamePlayer0
        playerPosY0 = pPosY gamePlayer0


-- Player Functions
checkPlayerStartFalling :: GameLevel -> GameSession -> Bool
checkPlayerStartFalling gameLevel gameSession = not $ oDrivable $ getPlayerGameObject gameLevel gameSession 0 (-1)

checkPlayerStopFalling :: GameLevel -> GameSession -> Bool
checkPlayerStopFalling gameLevel gameSession = oDrivable $ getPlayerGameObject gameLevel gameSession 0 0

getPlayerGameObject :: GameLevel -> GameSession -> Int -> Int -> GameObject
getPlayerGameObject gameLevel gameSession offsetX offsetY = oObjects gameColumn !! (gamePlayerY + offsetY)
    where
        gameColumn = gameLevel !! (gamePosX + gamePlayerX + offsetX)
        gamePosX = floor $ gPosX gameSession
        gamePlayer = gPlayer gameSession
        gamePlayerX = floor $ pPosX gamePlayer
        gamePlayerY = floor $ pPosY gamePlayer
