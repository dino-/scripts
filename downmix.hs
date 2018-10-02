#! /usr/bin/env stack
{- stack runghc -}

-- Script for downmixing a 5.1 (or whatever) video file to two channels (stereo)

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.Printf


main :: IO ()
main = do
  inputPath <- getArgs >>= return . head >>= makeAbsolute
  let outputPath = insertBeforeExtension "_ac2" inputPath

  (system $ printf "ffmpeg -i \"%s\" -map 0:0 -map 0:1 -map 0:1 -c:v copy -c:a:0 aac -b:a:0 192k -ac 2 -c:a:1 copy \"%s\""
    (inputPath :: FilePath) (outputPath :: FilePath)) >>= exitWith


insertBeforeExtension :: String -> FilePath -> FilePath
insertBeforeExtension part oldPath = addExtension (path ++ part) ext
  where (path, ext) = splitExtension oldPath
