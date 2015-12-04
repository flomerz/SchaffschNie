module Game.Output.Audio
    ( playSound
    , stopSound
    ) where

import Control.Concurrent.MVar
import System.Process


soundDir :: String
soundDir = "res/sounds/"

soundExtension :: String
soundExtension = ".mp3"


playSound :: Int -> MVar ProcessHandle -> IO ()
playSound sound audioProcessVar = do
    stopSound audioProcessVar

    let file = soundDir ++ (show sound) ++ soundExtension
    audioProcess <- spawnProcess "cvlc" ["-q", "--play-and-exit", file]
    _ <- tryPutMVar audioProcessVar audioProcess
    return ()

stopSound :: MVar ProcessHandle -> IO ()
stopSound audioProcessVar = do
    maybeAudioProcess <- tryTakeMVar audioProcessVar
    case maybeAudioProcess of
        Just audioProcess -> do
            terminateProcess audioProcess
        _ -> return ()
