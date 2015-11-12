module AppTypes where

import Output.Shapes
import Input.Events


-- TYPE DEFINITIONS
data AppInput = AppInput { inpMouse :: MouseEvent
                         , inpKey :: KeyEvent
                         , inpQuit :: Bool
                         }

data AppOutput = AppOutput { renderObject :: RenderObject
                           , exit :: Bool
                           }


-- TYPE INITIALIZERS
initAppInput :: AppInput
initAppInput = AppInput { inpMouse  = Nothing
                        , inpKey    = Nothing
                        , inpQuit   = False
                        }