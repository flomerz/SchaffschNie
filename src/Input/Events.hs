module Input.Events where

data InputEvent = Key KeyEvent
                | Mouse MouseEvent
                | Quit
                | NoInput

type KeyEvent = Maybe Key

type MouseEvent = Maybe MouseButton

data Key = KeySpace
         | Key1
         | Key2
         | Key3

data MouseButton = Left
                 | Right
