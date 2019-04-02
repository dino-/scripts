#! /usr/bin/env runhaskell

module Main where

import System.Environment
import Text.Printf


-- Display usage info for this script.
displayUsage :: IO ()
displayUsage = putStrLn $ unlines
   [ "Usage: perc.hs [OPTIONS] LOW HIGH"
   , "Calculate what percentage LOW is of (LOW + HIGH)"
   , ""
   , "Options:"
   , "  -h, --help  This usage information"
   , ""
   , "Dino Morelli <dino@ui3.info>"
   ]


main :: IO ()
main = do
   -- Get list of string args from the env
   argStrings <- getArgs

   -- Examine the number of arguments given
   case (length argStrings) of
      2 -> do
         -- Convert to numeric type and bind them
         let (lo : hi : []) = map read argStrings

         let sum' = lo + hi
         let perc = lo * 100 / sum'

         printf "%.2f is %.2f%% of %.2f\n"
            (lo :: Float) (perc :: Float) (sum' :: Float)

      _ -> displayUsage
