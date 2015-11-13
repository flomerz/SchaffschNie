module GameCore where

import FRP.Yampa

import Control.Monad
import Control.Concurrent

import qualified Input.Core as Input
import qualified Output.Core as Output

import AppTypes


run :: IO ()
run = do
        graficsEnv@(_, renderer) <- Output.init (300,300) "Test"
        startYampa (fmap Event Input.input) (Output.output renderer) Input.getTime
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

            yampaOutput :: Bool -> AppOutput -> IO Bool
            yampaOutput changed appOutput = do
                    when changed $ outputFunction $ renderObject appOutput
                    return $ exit appOutput

            yampaSignalFunction :: SF AppInputEvent AppOutput
            yampaSignalFunction = constant $ AppOutput (Output.rectangle_ (50, 50)) False

        reactimate yampaInitial yampaInput yampaOutput yampaSignalFunction


getTimeDelta :: Fractional a => a -> MVar a -> IO a
getTimeDelta currentTime timeMVar = (currentTime -) <$> swapMVar timeMVar currentTime
