module Game.Output.Audio
    ( playSound
    , stopSound
    ) where

import Control.Concurrent.MVar
import System.Process
import System.Process.Internals
import System.Posix.Signals


soundDir :: String
soundDir = "res/sounds/"

soundExtension :: String
soundExtension = ".mp3"


playSound :: Int -> MVar ProcessHandle -> IO ()
playSound sound audioProcessVar = do
    stopSound audioProcessVar

    let file = soundDir ++ (show sound) ++ soundExtension
    audioProcess <- spawnProcess "cvlc" [file]
    _ <- tryPutMVar audioProcessVar audioProcess
    return ()

stopSound :: MVar ProcessHandle -> IO ()
stopSound audioProcessVar = do
    maybeAudioProcess <- tryTakeMVar audioProcessVar
    case maybeAudioProcess of
        Just audioProcess -> do
            terminateProcessUnix audioProcess
        _ -> return ()


terminateProcessUnix :: ProcessHandle -> IO ()
terminateProcessUnix ph = do
    let (ProcessHandle pmvar _) = ph
    ph_ <- readMVar pmvar
    case ph_ of
        OpenHandle pid -> do -- pid is a POSIX pid
            signalProcess 9 pid
        _ -> return ()
