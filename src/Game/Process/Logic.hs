 {-# LANGUAGE Arrows            #-}
module Game.Process.Logic where

import FRP.Yampa

import Game.AppTypes
import Game.Types
import Game.Process.Event


import Debug.Trace


worldSpeed :: Double
worldSpeed = 10

jumpSpeed :: Double
jumpSpeed = 25


-- Main Function
gameSF :: GameData -> SF AppInput GameData
gameSF gameData0 = switch sf newLevel
    where
        sf = proc appInput -> do
            nextSession <- gameSessionSF (gameLevel, gameSession0) -< appInput
            let nextGameData = gameData0 { gSession = nextSession }
            -- take key 1, 2, 3 event for level selection
            returnA -< (nextGameData, NoEvent)

        newLevel number = gameSF nextGameData
            where nextGameData = gameData0 { gSession = gameSession0 { gLevel = number} }

        gameLevel = (currentGameLevel gameData0)
        gameSession0 = (gSession gameData0)



-- Game Session
gameSessionSF :: GameDataSession -> SF AppInput GameSession
gameSessionSF gameDataSession0@(gameLevel, gameSession0) = switch sf restartLevel
    where
        sf = proc appInput -> do
            gameSession <- moveWorldSF gameSession0 -< ()

            nextGamePlayer <- playerFallingSF gameLevel gamePlayer0 -< (appInput, gameSession)
            let nextGameSession = gameSession { gPlayer = nextGamePlayer }

            -- check events
            endOfWorldEvent <- edge -< checkEndOfWorld (gameLevel, nextGameSession)
            collisionEvent <- edge -< checkCollision (gameLevel, nextGameSession)
            let restartLevelEvent = rMerge endOfWorldEvent collisionEvent

            returnA -< (nextGameSession, restartLevelEvent)
            where gamePlayer0 = gPlayer gameSession0

        restartLevel _ = gameSessionSF gameDataSession0


-- Game Session Functions
checkEndOfWorld :: GameDataSession -> Bool
checkEndOfWorld (gameLevel, gameSession) = endReached
    where endReached = (getEndPositionOfLevel <= gPosX gameSession)
          getEndPositionOfLevel = (oPositionX $ last gameLevel) - 5

checkCollision :: GameDataSession -> Bool
checkCollision gameDataSession = oColliding frontObj || oColliding topObj
    where
        frontObj = getPlayerGameObject gameDataSession 1 0
        topObj = getPlayerGameObject gameDataSession 0 1


-- World SF
moveWorldSF :: GameSession -> SF a GameSession
moveWorldSF (gameSession0) = proc _ -> do
        nextPosX <- moveWorldPositionSF (gPosX gameSession0) -< ()
        returnA -< gameSession0 { gPosX = nextPosX }

moveWorldPositionSF :: Double -> SF () Double
moveWorldPositionSF gPosX0 = constant worldSpeed >>> imIntegral gPosX0



-- Player SF State
playerDrivingSF :: GameLevel -> GamePlayer -> SF (AppInput, GameSession) GamePlayer
playerDrivingSF gameLevel gamePlayer0 = trace ("drive " ++ show gamePlayer0) $ switch sf (\gamePlayer -> playerFallingSF gameLevel gamePlayer)
    where
        sf = proc (appInput, gameSession) -> do
            let nextGameSession = gameSession { gPlayer = gamePlayer0 }

            -- check events
            playerFallingEvent <- tagWith gamePlayer0 ^<< edge -< checkPlayerStartFalling (gameLevel, nextGameSession)
            playerJumpEvent <- tagWith jumpGamePlayer ^<< jumpTrigger -< appInput
            let playerInTheAirEvent = lMerge playerFallingEvent playerJumpEvent

            returnA -< (gamePlayer0, playerInTheAirEvent)

        jumpGamePlayer = gamePlayer0 { pV = jumpSpeed }


playerFallingSF :: GameLevel -> GamePlayer -> SF (AppInput, GameSession) GamePlayer
playerFallingSF gameLevel gamePlayer0 = trace ("fall " ++ show gamePlayer0) $ switch sf (\gamePlayer -> playerDrivingSF gameLevel gamePlayer)
    where
        sf = proc (_, gameSession) -> do
            let playerAccelerationY = oAccelerationY $ getPlayerGameObject (gameLevel, gameSession) 0 0
            nextPlayer <- fallPlayer gamePlayer0 -< playerAccelerationY
            let nextGameSession = gameSession { gPlayer = nextPlayer }

            -- check event
            playerDrivingEvent <- edge -< checkPlayerStopFalling (gameLevel, nextGameSession)

            returnA -< (nextPlayer, playerDrivingEvent `tag` ajustPlayer nextPlayer)
            where
                ajustPlayer player = player { pPosY = (fromInteger . ceiling $ (pPosY player)) , pV = 0}


-- Player Functions
checkPlayerStartFalling :: GameDataSession -> Bool
checkPlayerStartFalling gameDataSession = not $ oDrivable $ getPlayerGameObject gameDataSession 0 (-1)

checkPlayerStopFalling :: GameDataSession -> Bool
checkPlayerStopFalling gameDataSession = oDrivable $ getPlayerGameObject gameDataSession 0 0

getPlayerGameObject :: GameDataSession -> Int -> Int -> GameObject
getPlayerGameObject (gameLevel, gameSession) offsetX offsetY = oObjects gameColumn !! (gamePlayerY + offsetY)
    where
        gameColumn = gameLevel !! (gamePosX + gamePlayerX + offsetX)
        gamePosX = floor $ gPosX gameSession
        gamePlayer = gPlayer gameSession
        gamePlayerX = floor $ pPosX gamePlayer
        gamePlayerY = floor $ pPosY gamePlayer


-- Player SF
fallPlayer :: GamePlayer -> SF Double GamePlayer
fallPlayer gamePlayer0 = proc accelerationY -> do
    nextV <- imIntegral playerV0 -< -accelerationY
    nextY <- imIntegral playerPosY0 -< nextV
    returnA -< gamePlayer0 { pV = nextV
                           , pPosY = nextY
                           }
    where
        playerV0 = pV gamePlayer0
        playerPosY0 = pPosY gamePlayer0
