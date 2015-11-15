{-# LANGUAGE Arrows            #-}
module Game.Process.Logic where

import FRP.Yampa

import Game.AppTypes
import Game.Types


game :: GameData -> SF AppInput GameLevel
game gameData = constant $ gameData
