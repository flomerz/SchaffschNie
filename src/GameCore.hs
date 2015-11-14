module GameCore where

import Control.Monad
import Control.Concurrent

import qualified FRP.Yampa as Yampa
import FRP.Yampa ( Event(..)
                 , DTime
                 , SF
                 , (>>>)
                 )

import qualified Input.Core as Input
import qualified Output.Core as Output

import qualified GameLogic
import AppTypes


run :: IO ()
run = do
        graficsEnv <- Output.init (300,300) "Test"
        startYampa (fmap Event Input.input) (Output.output graficsEnv) Input.getTime
        Output.quit graficsEnv


startYampa :: (IO AppInputEvent)
            -> (Output.RenderObject -> IO ())
            -> IO Double
            -> IO ()
startYampa inputFunction outputFunction timeFunction = do        
        timeMVar <- newMVar =<< timeFunction

        let
            yampaInitial :: IO AppInputEvent
            yampaInitial = return NoEvent

            yampaInput :: Bool -> IO (DTime, Maybe AppInputEvent)
            yampaInput _canBlock = do
                    currentTime <- timeFunction
                    deltaTime <- getTimeDelta currentTime timeMVar
                    appInputEvent <- inputFunction
                    return (deltaTime, Just appInputEvent)
                    where
                        getTimeDelta :: Fractional a => a -> MVar a -> IO a
                        getTimeDelta currentTime mVar = (currentTime -) <$> swapMVar mVar currentTime

            yampaOutput :: Bool -> AppOutput -> IO Bool
            yampaOutput changed appOutput = do
                    when changed $ outputFunction $ renderObject appOutput
                    return $ exit appOutput

            yampaSignalFunction :: SF AppInputEvent AppOutput
            yampaSignalFunction = accumulateInput >>> GameLogic.game
                    where 
                        accumulateInput :: SF AppInputEvent AppInput
                        accumulateInput = Yampa.accumHoldBy accumulateEvent initAppInput

        Yampa.reactimate yampaInitial yampaInput yampaOutput yampaSignalFunction
