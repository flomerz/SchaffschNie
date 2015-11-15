{-# LANGUAGE Arrows            #-}
module Game.Process.Event where

import FRP.Yampa

import Game.AppTypes
import Game.Input.Events


keyPress :: SF AppInput (Event Key)
keyPress = inpKey ^>> edgeJust

keyPressed :: Key -> SF AppInput (Event ())
keyPressed key = keyPress >>^ filterE (key ==) >>^ tagWith ()


