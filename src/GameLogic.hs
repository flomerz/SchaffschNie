{-# LANGUAGE Arrows            #-}
module GameLogic where

import FRP.Yampa

import AppTypes
import Input.Events
import Output.Shapes


game :: SF AppInput AppOutput
game = gamesf $ scene_ [ rectangle_ (50, 50) & colour_ (sRGB24 0x1A 0xAF 0x5D)
                       , rectangle_ (100, 10) & pos_ (100,0) & colour_ (sRGB24 0xFF 0x00 0x00)
                       ]

gamesf :: RenderObject -> SF AppInput AppOutput
gamesf objs = switch sf (\_ -> gamesf $ objs {objType = Multiple [head subObjs & pos_ (x, y)]})
        where 
            sf = proc input -> do
                let quit = inpQuit input
                keyEvent <- keyPressed KeySpace -< input
                returnA -< (AppOutput objs quit, keyEvent)
            (x, y) = objPos $ head subObjs
            (Multiple subObjs) = objType objs

keyPress :: SF AppInput (Event Key)
keyPress = inpKey ^>> edgeJust

keyPressed :: Key -> SF AppInput (Event ())
keyPressed key = keyPress >>^ filterE (key ==) >>^ tagWith ()
