module Game.Input.Events where

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
         deriving (Show, Eq)

data MouseButton = Left
                 | Right
                 deriving (Show, Eq)
