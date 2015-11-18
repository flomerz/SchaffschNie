module Game.Util where

toupleF :: (a -> b) -> (a, a) -> (b, b)
toupleF f (a1, a2) = (f a1, f a2)
