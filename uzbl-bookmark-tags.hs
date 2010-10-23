#! /usr/bin/env runhaskell

import Data.List
import Data.Maybe
import System.Environment
import System.FilePath
import Text.Regex


bookmarksPath :: IO FilePath
bookmarksPath = do
   home <- getEnv "HOME"
   return $ home </> ".local/share/uzbl/bookmarks"


readBookmarkLines :: IO [String]
readBookmarkLines = fmap lines $ bookmarksPath >>= readFile


eachBookmarksTags :: [String] -> [String]
eachBookmarksTags = concat . catMaybes .
   map (matchRegex (mkRegex ".*\t(.*)"))


main :: IO ()
main = do
   bls <- readBookmarkLines
   let tags = eachBookmarksTags bls
   mapM_ putStrLn $ sort tags
