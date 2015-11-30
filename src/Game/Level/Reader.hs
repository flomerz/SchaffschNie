module Game.Level.Reader
    ( read
    ) where

import Prelude hiding (read)

import Data.List

import Game.Types
import Game.Level.Parser


levelPath :: FilePath
levelPath = "res/lvls/"

read :: Int -> IO GameLevel
read lvl = fmap (parseLevel . transform) $ readFile (levelPath ++ (show lvl) ++ ".txt")

transform :: String -> [ObjectSignColumn]
transform str = addPosition . transpose . reverse . lines $ str
         where addPosition lvl = zip (map (\x -> zip x [0..]) lvl) [0..]
