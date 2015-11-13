-- Thanks to Konstantin Zudov

module Output.Shapes
    ( Shape(..)
    , ObjectType(..)
    , RenderObject(..)
    , def
    , scene_
    , circle_
    , line_
    , rectangle_
    , text_
    , pos_
    , colour_
    , Colour
    , sRGB24
    , (&)
    ) where

import Data.Default
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names


data Shape = Circle Int
           | Rectangle (Double, Double)
           | Line Int Double
           | TextRectangle Int Int String
           deriving (Show, Eq)

data ObjectType = Single { objShape :: Shape }
                | Multiple [RenderObject]
                deriving (Show, Eq)

data RenderObject = RenderObject { objType      :: ObjectType
                                 , objPos       :: (Double, Double)
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

rectangle_ :: (Double, Double) -> RenderObject
rectangle_ size = def { objType = Single $ Rectangle size }

line_ :: Int -> Double -> RenderObject
line_ length_ angle = def { objType = Single $ Line length_ angle }

text_ :: Double -> Double -> String -> RenderObject
text_ x y txt = def { objType = Single $ TextRectangle (round x) (round y) txt }

type AttributeSetter = RenderObject -> RenderObject

pos_ :: (Double, Double) -> AttributeSetter
pos_ pos obj = obj { objPos = pos }

colour_ :: Colour Double -> AttributeSetter
colour_ colour obj = obj { objColour = colour }

(&) :: RenderObject -> AttributeSetter -> RenderObject
(&) = flip ($)
