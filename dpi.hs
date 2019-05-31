#! /usr/bin/env stack
-- stack runghc

module Main where

import System.Environment
import Text.Printf


-- Display usage info for this script.
displayUsage :: IO ()
displayUsage = putStrLn $ unlines
   [ "Usage: dpi.hs [OPTIONS] WIDTH HEIGHT DIAG"
   , "Calculate DPI (dots per inch) from screen resolution and screen size."
   , ""
   , "  WIDTH and HEIGHT: in pixels"
   , "  DIAG: measurement of screen in inches"
   , ""
   , "Options:"
   , "  -h, --help  This usage information"
   , ""
   , "v1.1  2019-05-31  Dino Morelli <dino@ui3.info>"
   ]


main :: IO ()
main = do
   -- Get list of string args from the env
   argStrings <- getArgs

   -- Examine the number of arguments given
   case (length argStrings) of
      3 -> do
         -- Convert to numeric type and bind them
         let (wp : hp : di : []) = map read argStrings

         -- Calculate dpi from the input
         let dpi = (sqrt ((wp ^ 2) + (hp ^ 2))) / di

         -- Display results
         printf "   resolution (pixels, W x H): %.0f x %.0f\n"
            (wp::Float) (hp::Float)
         printf "diagonal screen size (inches): %.1f\n" (di::Float)
         printf "          dots per inch (DPI): %.2f\n" (dpi::Float)

      _ -> displayUsage
