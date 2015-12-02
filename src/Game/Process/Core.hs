{-# LANGUAGE Arrows            #-}
module Game.Process.Core (run) where

import FRP.Yampa

import Game.AppTypes
import Game.Types
import Game.Input.Events
import Game.Output.Shapes
import qualified Game.Process.Logic as Logic
import qualified Game.Process.Renderer as Renderer


run :: ResolutionSettings -> GameData -> SF AppInputEvent AppOutput
run resSettings gameData0 = proc appInputEvent -> do
    appInput <- accumulateInput -< appInputEvent
    gameData <- Logic.gameSF gameData0 -< appInput
    exit <- handleExit -< appInput
    t <- time -< ()
    let renderObject = Renderer.render resSettings (t, gameData)
    let audio = playAudio gameData
    returnA -< reduceOutput renderObject audio exit

accumulateInput :: SF AppInputEvent AppInput
accumulateInput = accumHoldBy accumulateEvent initAppInput

accumulateEvent :: AppInput -> InputEvent -> AppInput
accumulateEvent appInput inputEvent = case inputEvent of
    Key keyEvent        -> appInput { inpKey = keyEvent }
    Mouse mouseEvent    -> appInput { inpMouse = mouseEvent }
    Quit                -> appInput { inpQuit = True }
    NoInput             -> appInput

handleExit :: SF AppInput Bool
handleExit = arr inpQuit

playAudio :: GameData -> PlayAudio
playAudio gameData = if (gPosX $ gSession gameData) == 0 then Just (gLevel $ gSession gameData) else Nothing

reduceOutput :: RenderObject -> PlayAudio -> Bool -> AppOutput
reduceOutput renderObj audio exit = initAppOutput { outRenderObject = renderObj
                                                  , outExit         = exit
                                                  , outAudio        = audio
                                                  }
