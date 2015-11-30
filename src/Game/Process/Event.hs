{-# LANGUAGE Arrows            #-}
module Game.Process.Event where

import FRP.Yampa
import FRP.Yampa.Event

import Game.AppTypes
import Game.Input.Events


keyPress :: SF AppInput (Event Key)
keyPress = arr (maybeToEvent . inpKey)

keyPressed :: Key -> SF AppInput (Event ())
keyPressed key = keyPress >>^ filterE (key ==) >>^ tagWith ()


jumpTrigger :: SF AppInput (Event())
jumpTrigger = proc input -> do
    spacebarTab <- keyPressed KeySpace -< input
    returnA -< spacebarTab
