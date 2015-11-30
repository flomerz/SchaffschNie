{-# LANGUAGE Arrows            #-}
module Game.Process.Event where

import FRP.Yampa
import FRP.Yampa.Event

import Game.AppTypes
import Game.Input.Events


keyPress :: SF AppInput (Event Key)
keyPress = arr (maybeToEvent . inpKey)

edgeKeyPress :: SF AppInput (Event Key)
edgeKeyPress = inpKey ^>> edgeJust

keyPressed :: Key -> SF AppInput (Event ())
keyPressed key = keyPress >>^ filterE (key ==) >>^ tagWith ()

edgeKeyPressed :: Key -> SF AppInput (Event ())
edgeKeyPressed key = edgeKeyPress >>^ filterE (key ==) >>^ tagWith ()


levelTrigger :: SF AppInput (Event Int)
levelTrigger = proc input -> do
    key1Pressed <- tagWith 1 ^<< edgeKeyPressed Key1 -< input
    key2Pressed <- tagWith 2 ^<< edgeKeyPressed Key2 -< input
    key3Pressed <- tagWith 3 ^<< edgeKeyPressed Key3 -< input
    returnA -< mergeEvents [key1Pressed, key2Pressed, key3Pressed]

jumpTrigger :: SF AppInput (Event())
jumpTrigger = proc input -> do
    spacebarTab <- keyPressed KeySpace -< input
    returnA -< spacebarTab
