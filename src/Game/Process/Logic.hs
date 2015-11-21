{-# LANGUAGE Arrows            #-}
module Game.Process.Logic where

import FRP.Yampa

import Game.AppTypes
import Game.Types
import Game.Process.Event
import Game.Input.Events

import Debug.Trace

moveWorld :: GameSession -> SF AppInput GameSession
moveWorld (GameSession gPlayer0 gLevel0 gPosX0 gameTries) = proc _ -> do
    nextPosX <- imIntegral gPosX0 -< 5
    returnA -< (GameSession gPlayer0 gLevel0 nextPosX gameTries)

checkEndOfWorld :: GameData -> Bool
checkEndOfWorld gameData@(GameData lvls (GameSession _ currentLvl currentPosX _)) = endReached
    where endReached = (getEndPositionOfLevel <= currentPosX)
          getEndPositionOfLevel = oPositionX $ getGameObjectLastColumn
          getGameObjectLastColumn = last getCurrentLvl
          getCurrentLvl = lvls !! (currentLvl - 1)

movements :: GameSession -> SF AppInput GameSession
movements currentSession = switch sf cont 
    where 
        sf = proc input -> do
            nextSession <- moveWorld currentSession -< input
            jump <- keyPressed KeySpace -< input
            returnA -< (nextSession, jump `tag`nextSession)
        cont nextSession = movements nextSession
{-
jumpPlayer :: GameSession -> SF AppInput GameSession
jumpPlayer (GameSession _ _ _) = proc input -> do
    gameData <- jumpTrigger -< input
    returnA -< (GameData gameData)

jumpTrigger :: SF AppInput (Event())
jumpTrigger = proc input -> do
    spacebarTab <- keypressed ScancodeSpace -< input
    returnA -< spacebarTab

-}
game :: GameData -> SF AppInput GameData
game gameData = switch sf (\_ -> game gameData)
    where
        sf = proc input -> do
            nextSession <- moveWorld currentSession -< input
            let nextGameData = gameData { gSession = nextSession }
            ev <- edge -< checkEndOfWorld nextGameData
            returnA -< (nextGameData, ev)
            where currentSession = gSession gameData
