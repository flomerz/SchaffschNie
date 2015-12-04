module Game.Core where

import Control.Monad
import Control.Concurrent

import qualified FRP.Yampa as Yampa
import FRP.Yampa ( Event(..)
                 , DTime
                 , SF
                 )

import Game.AppTypes
import Game.Types

import qualified Game.Input.Core as Input
import qualified Game.Output.Core as Output
import qualified Game.Output.Audio as Audio

import qualified Game.Level.Reader as Level
import qualified Game.Process.Core as Process


windowSize :: (Int, Int)
-- windowSize = (1024, 640)
windowSize = (1280, 768)

renderScale :: Double
renderScale = 48

windowTitle :: String
windowTitle = "Schaffsch Nie"

worldSpeed :: Double
worldSpeed = 15

jumpSpeed :: Double
jumpSpeed = 25

run :: IO ()
run = do
        graficsEnv <- Output.init windowSize windowTitle

        levels <- mapM Level.read [1, 2, 3]
        let gameSession = initGameSession initGamePlayer 1
        let gameData = initGameData gameSession (GameSettings windowSize renderScale worldSpeed jumpSpeed) levels

        fpsCounter <- newMVar (0::Integer)
        fpsLastTicks <- newMVar (0::Integer)
        audioProcessVar <- newEmptyMVar

        startYampa (Event <$> Input.input)
                   (Output.output (fpsCounter, fpsLastTicks, audioProcessVar) graficsEnv)
                   Input.getTime
                   (Process.run (windowSize, renderScale) gameData)

        Audio.stopSound audioProcessVar

        Output.quit graficsEnv


startYampa :: IO AppInputEvent                  -- input function
            -> (AppOutput -> IO ())   -- output function
            -> IO Double                        -- time function
            -> SF AppInputEvent AppOutput       -- process function
            -> IO ()
startYampa inputFunction outputFunction timeFunction processFunction = do
        timeMVar <- newMVar =<< timeFunction

        let
            yampaInitial :: IO AppInputEvent
            yampaInitial = return NoEvent

            yampaInput :: Bool -> IO (DTime, Maybe AppInputEvent)
            yampaInput _canBlock = do
                    deltaTime <- getTimeDelta timeMVar =<< timeFunction
                    appInputEvent <- inputFunction
                    return (deltaTime, Just appInputEvent)
                    where
                        getTimeDelta :: Fractional a => MVar a -> a -> IO a
                        getTimeDelta mVar currentTime = (currentTime -) <$> swapMVar mVar currentTime

            yampaOutput :: Bool -> AppOutput -> IO Bool
            yampaOutput changed appOutput = do
                    when changed $ outputFunction appOutput
                    return $ outExit appOutput

        Yampa.reactimate yampaInitial yampaInput yampaOutput processFunction
