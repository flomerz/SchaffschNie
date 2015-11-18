-- Thanks to Konstantin Zudov

module Game.Output.Shapes
    ( Shape(..)
    , ImageType(..)
    , ObjectType(..)
    , RenderObject(..)
    , def
    , scene_
    , circle_
    , line_
    , rectangle_
    , image_
    , text_
    , pos_
    , posX_
    , posY_
    , colour_
    , Colour
    , sRGB24
    , (&)
    ) where

import Data.Default
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names

data ImageType = PlayerImage
               | AirImage
               | BoxImage
               | LavaImage
               deriving (Show, Eq)

type ShapeSize = (Double, Double)
type ShapePosition = (Double, Double)
type ShapeSprite = Maybe (ShapePosition, ShapeSize)

data Shape = Circle Int
           | Rectangle ShapeSize
           | Line Int Double
           | TextRectangle Int Int String
           | Image ImageType ShapeSize ShapeSprite
           deriving (Show, Eq)

data ObjectType = Single { objShape :: Shape }
                | Multiple [RenderObject]
                deriving (Show, Eq)

data RenderObject = RenderObject { objType      :: ObjectType
                                 , objPos       :: ShapePosition
                                 , objColour    :: Colour Double
                                 } deriving (Show, Eq)



instance Default RenderObject where
        def = RenderObject { objType    = error "RenderObject type wasn't define"
                           , objPos     = (0, 0)
                           , objColour  = white
                           }

-- Terms are written with postfix '_' indicating data rather than code.
-- (stolen from lucid)

-- It might worth to use lenses here in order to avoid building a
-- poor version of them

scene_ :: [RenderObject] -> RenderObject
scene_ objs = def { objType = Multiple objs, objColour = black}

circle_ :: Double -> RenderObject
circle_ n = def { objType = Single $ Circle (round n) }

rectangle_ :: ShapeSize -> RenderObject
rectangle_ size = def { objType = Single $ Rectangle size }

image_ :: ImageType -> ShapeSize -> ShapeSprite -> RenderObject
image_ imgType size stripe = def { objType = Single $ Image imgType size stripe }

line_ :: Int -> Double -> RenderObject
line_ length_ angle = def { objType = Single $ Line length_ angle }

text_ :: Double -> Double -> String -> RenderObject
text_ x y txt = def { objType = Single $ TextRectangle (round x) (round y) txt }

type AttributeSetter = RenderObject -> RenderObject

pos_ :: ShapePosition -> AttributeSetter
pos_ pos obj = obj { objPos = pos }

posX_ :: Double -> AttributeSetter
posX_ posX obj = obj { objPos = (posX, posY) }
    where (_,posY) = objPos obj

posY_ :: Double -> AttributeSetter
posY_ posY obj = obj { objPos = (posX, posY) }
    where (posX,_) = objPos obj

colour_ :: Colour Double -> AttributeSetter
colour_ colour obj = obj { objColour = colour }

(&) :: RenderObject -> AttributeSetter -> RenderObject
(&) = flip ($)
