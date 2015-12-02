module Game.AppTypes where

import FRP.Yampa.Event

import Game.Output.Shapes
import Game.Input.Events


-- TYPE DEFINITIONS
type AppInputEvent = Event InputEvent

data AppInput = AppInput { inpMouse     :: MouseEvent
                         , inpKey       :: KeyEvent
                         , inpQuit      :: Bool
                         } deriving (Show)


type PlayAudio = Maybe Int

data AppOutput = AppOutput { outRenderObject   :: RenderObject
                           , outAudio          :: PlayAudio
                           , outExit           :: Bool
                           }


-- TYPE INITIALIZERS
initAppInput :: AppInput
initAppInput = AppInput { inpMouse  = Nothing
                        , inpKey    = Nothing
                        , inpQuit   = False
                        }

initAppOutput :: AppOutput
initAppOutput = AppOutput { outRenderObject     = error "no RenderObject defined"
                          , outAudio            = Nothing
                          , outExit             = False
                          }
