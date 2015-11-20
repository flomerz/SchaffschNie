module Game.Output.Shapes where

import Data.Default


type ShapeSize = (Double, Double)
type ShapePosition = (Double, Double)
type ShapeSprite = Maybe (ShapePosition, ShapeSize)

data Shape = Text String Int
           | Image String ShapeSize ShapeSprite
           deriving (Show, Eq)

data ObjectType = Single { objShape :: Shape }
                | Multiple [RenderObject]
                deriving (Show, Eq)

data RenderObject = RenderObject { objType      :: ObjectType
                                 , objPos       :: ShapePosition
                                 } deriving (Show, Eq)



instance Default RenderObject where
        def = RenderObject { objType    = error "RenderObject type wasn't define"
                           , objPos     = (0, 0)
                           }

-- Terms are written with postfix '_' indicating data rather than code.
-- (stolen from lucid)

-- It might worth to use lenses here in order to avoid building a
-- poor version of them

scene_ :: [RenderObject] -> RenderObject
scene_ objs = def { objType = Multiple objs}

image_ :: String -> ShapeSize -> ShapeSprite -> RenderObject
image_ imgName size stripe = def { objType = Single $ Image imgName size stripe }

text_ :: String -> Int -> RenderObject
text_ txt size = def { objType = Single $ Text txt size }

type AttributeSetter = RenderObject -> RenderObject

pos_ :: ShapePosition -> AttributeSetter
pos_ pos obj = obj { objPos = pos }

posX_ :: Double -> AttributeSetter
posX_ posX obj = obj { objPos = (posX, posY) }
    where (_,posY) = objPos obj

posY_ :: Double -> AttributeSetter
posY_ posY obj = obj { objPos = (posX, posY) }
    where (posX,_) = objPos obj

(&) :: RenderObject -> AttributeSetter -> RenderObject
(&) = flip ($)
