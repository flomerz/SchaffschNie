module Main where


import qualified FRP.Yampa as Yampa

import qualified GameReader
import qualified Output.Graphics as Output
import qualified Output.Shapes as Shapes
import Control.Concurrent


main :: IO ()
main = do
    env@(window, renderer) <- Output.init (300,300) "Test"
    Output.render renderer $ ((Shapes.rectangle_ (100, 50)) Shapes.& (Shapes.colour_ $ Shapes.sRGB24 0x1A 0xAF 0x5D))
    threadDelay(3000000)
    Output.quit env

startYampa :: IO ()
startYampa = do
             gameWorld <- GameReader.read 1
             Yampa.reactimate (return 5) (\_ -> return (1, Just 1)) (\_ b -> do putStrLn $ show b; return True) $ Yampa.identity


-- Yampa.reactimate
--   :: IO a
--      -> (Bool -> IO (Yampa.DTime, Maybe a))
--      -> (Bool -> b -> IO Bool)
--      -> Yampa.SF a b
--      -> IO ()

