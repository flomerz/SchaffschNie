module Game.Core where

import Control.Monad
import Control.Concurrent

import qualified FRP.Yampa as Yampa
import FRP.Yampa ( Event(..)
                 , DTime
                 , SF
                 , (>>>)
                 , (&&&)
                 )

import Game.AppTypes

import qualified Game.Input.Core as Input
import Game.Input.Events
import qualified Game.Output.Core as Output

import qualified Game.Level.Reader as Level
import qualified Game.Process.Logic as Logic


run :: IO ()
run = do
        graficsEnv <- Output.init (300,300) "Test"
        startYampa (Event <$> Input.input) (Output.output graficsEnv) Input.getTime
        Output.quit graficsEnv


startYampa :: (IO AppInputEvent)
            -> (Output.RenderObject -> IO ())
            -> IO Double
            -> IO ()
startYampa inputFunction outputFunction timeFunction = do
        timeMVar <- newMVar =<< timeFunction

        lvl <- Level.read 1

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
                    when changed $ outputFunction $ outRenderObject appOutput
                    return $ outExit appOutput

            yampaSignalFunction :: SF AppInputEvent AppOutput
            yampaSignalFunction = accumulateInput >>> Logic.gameLevel lvl &&& Yampa.identity >>> handleExit
                    where
                        accumulateInput :: SF AppInputEvent AppInput
                        accumulateInput = Yampa.accumHoldBy accumulateEvent initAppInput

                        accumulateEvent :: AppInput -> InputEvent -> AppInput
                        accumulateEvent appInput inputEvent = case inputEvent of
                            Key keyEvent        -> appInput { inpKey = keyEvent }
                            Mouse mouseEvent    -> appInput { inpMouse = mouseEvent }
                            Quit                -> appInput { inpQuit = True }
                            NoInput             -> appInput

                        handleExit :: SF (AppOutput, AppInput) AppOutput
                        handleExit = Yampa.arr (\(appOutput, appInput) -> appOutput { outExit = inpQuit appInput })


        Yampa.reactimate yampaInitial yampaInput yampaOutput yampaSignalFunction
