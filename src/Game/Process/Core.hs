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
run resSettings gameData = accumulateInput >>> (time &&& Logic.game gameData >>^ (Renderer.render resSettings)) &&& handleExit >>^ reduceOutput

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

reduceOutput :: (RenderObject, Bool) -> AppOutput
reduceOutput (renderObj, exit) = initAppOutput { outRenderObject = renderObj
                                               , outExit         = exit
                                               }
