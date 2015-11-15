module Game.AppTypes where

import FRP.Yampa.Event

import Game.Output.Shapes
import Game.Input.Events


-- TYPE DEFINITIONS
type AppInputEvent = Event InputEvent

data AppInput = AppInput { inpMouse     :: MouseEvent
                         , inpKey       :: KeyEvent
                         , inpQuit      :: Bool
                         }

data AppOutput = AppOutput { outRenderObject   :: RenderObject
                           , outExit           :: Bool
                           }


-- TYPE INITIALIZERS
initAppInput :: AppInput
initAppInput = AppInput { inpMouse  = Nothing
                        , inpKey    = Nothing
                        , inpQuit   = False
                        }

accumulateEvent :: AppInput -> InputEvent -> AppInput
accumulateEvent appInput inputEvent = case inputEvent of
        Key keyEvent        -> appInput { inpKey = keyEvent }
        Mouse mouseEvent    -> appInput { inpMouse = mouseEvent }
        Quit                -> appInput { inpQuit = True }
        NoInput             -> appInput