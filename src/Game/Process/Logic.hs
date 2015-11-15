{-# LANGUAGE Arrows            #-}
module Game.Process.Logic where

import FRP.Yampa

import Game.AppTypes
import Game.Input.Events
import Game.Output.Shapes
import Game.Types
import qualified Game.Process.Renderer as Renderer
import Game.Process.Event


gameLevel :: GameLevel -> SF AppInput AppOutput
gameLevel lvl = identity &&& sf >>> handleExit
    where sf = constant $ AppOutput (Renderer.render lvl) False

game :: SF AppInput AppOutput
game = identity &&& gamerenderertest >>> handleExit

handleExit :: SF (AppInput, AppOutput) AppOutput
handleExit = proc (appInput, appOutput) -> do
    returnA -< appOutput { outExit = inpQuit appInput }

gamerenderertest :: SF AppInput AppOutput
gamerenderertest = constant $ AppOutput (Renderer.render initGameObjectAir) False



gametest :: SF AppInput AppOutput
gametest = gamesf $ scene_ [ rectangle_ (50, 50) & colour_ (sRGB24 0x1A 0xAF 0x5D)
                           , rectangle_ (100, 10) & pos_ (100,100) & colour_ (sRGB24 0xFF 0x00 0x00)
                           ]

gamesf :: RenderObject -> SF AppInput AppOutput
gamesf objs = switch sf (\_ -> gamesf $ objs {objType = Multiple $ [head subObjs & pos_ (x+10, y+10)] ++ tail subObjs})
    where
        sf = proc input -> do
            let quit = inpQuit input
            keyEvent <- keyPressed KeySpace -< input
            returnA -< (AppOutput objs quit, keyEvent)
        (x, y) = objPos $ head subObjs
        (Multiple subObjs) = objType objs
