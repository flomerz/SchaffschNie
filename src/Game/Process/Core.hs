{-# LANGUAGE Arrows            #-}
module Game.Process.Core (run) where

import FRP.Yampa

import Game.AppTypes
import Game.Types
import Game.Input.Events
import Game.Output.Shapes
import qualified Game.Process.Logic as Logic
import qualified Game.Process.Renderer as Renderer


run :: GameData -> SF AppInputEvent AppOutput
run gameData = accumulateInput >>> processLogic gameData &&& handleExit >>> reduceOutput

accumulateInput :: SF AppInputEvent AppInput
accumulateInput = accumHoldBy accumulateEvent initAppInput

accumulateEvent :: AppInput -> InputEvent -> AppInput
accumulateEvent appInput inputEvent = case inputEvent of
    Key keyEvent        -> appInput { inpKey = keyEvent }
    Mouse mouseEvent    -> appInput { inpMouse = mouseEvent }
    Quit                -> appInput { inpQuit = True }
    NoInput             -> appInput

processLogic :: GameData -> SF AppInput RenderObject
processLogic gameData = Logic.game gameData >>> arr Renderer.render

handleExit :: SF AppInput Bool
handleExit = arr inpQuit

reduceOutput :: SF (RenderObject, Bool) AppOutput
reduceOutput = arr (\(renderObj, exit) -> initAppOutput { outRenderObject = renderObj
                                                        , outExit         = exit
                                                        })