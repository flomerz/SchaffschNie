module Game.Util where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))


toupleF :: (a -> b) -> (a, a) -> (b, b)
toupleF f (a1, a2) = (f a1, f a2)


getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    content <- getRecursiveContentsRec topdir
    return $ map removeTopDir content
    where
        getRecursiveContentsRec :: FilePath -> IO [FilePath]
        getRecursiveContentsRec dir = do
            names <- getDirectoryContents dir
            let properNames = filter (`notElem` [".", ".."]) names
            paths <- forM properNames $ \name -> do
                let path = dir </> name
                isDirectory <- doesDirectoryExist path
                if isDirectory
                    then getRecursiveContentsRec path
                    else return [path]
            return $ concat paths

        removeTopDir path = drop (length topdir) path