#! /usr/bin/env runhaskell

{- version: 1.0
-}

import System.Environment
import Text.Printf


main :: IO ()
main = do
   (path, indentChars) <- getArgs >>= parseArgs
   (firstLine:restOfLines) <- fmap lines $ readFile path

   outputLine indentChars '[' firstLine
   mapM_ (outputLine indentChars ',') restOfLines
   outputLine indentChars ']' ""


-- Simple parsing of various argument shapes
parseArgs :: [String] -> IO (FilePath, Int)
parseArgs ("-h"    : _      ) = usage
parseArgs ("--help": _      ) = usage
parseArgs (p       : i  : []) = return (p, read i)
parseArgs (p       : []     ) = return (p, 0)
parseArgs _                   = usage


-- General failure message, used by several patterns above
usage :: IO a
usage = error "usage: txt2hslist.hs FILE [INDENTSPACES]"


-- Output a line with a specified indent and specified first character
outputLine :: Int -> Char -> String -> IO ()
outputLine indent ']'       _    = printf "%s]\n" (spaces indent)
outputLine indent startChar line =
   printf "%s%c \"%s\"\n" (spaces indent) startChar line


-- Construct a String containing a specific number of spaces
spaces :: Int -> String
spaces i = replicate i ' '
