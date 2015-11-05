module GameReader
    ( read
    ) where

import Prelude hiding (read)

import Data.List
import Data.List.Split

import GameTypes
import GameParser


levelPath :: FilePath
levelPath = "res/lvls/"

read :: Int -> IO GameWorld
read lvl = fmap (parseGameWorld . transform) $ readFile (levelPath ++ "lvl" ++ (show lvl) ++ ".txt")

transform :: String -> [ObjectSignColumn]
transform str = addPosition . transpose . reverse $ splitOn "\n" str
                where addPosition lvl = zip (map (\x -> zip x [0..]) lvl) [0..]
