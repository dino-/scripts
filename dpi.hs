#! /usr/bin/runhaskell

module Main where

import System.Environment
import Text.Printf


-- Display usage info for this script.
displayUsage :: IO ()
displayUsage = do
   putStrLn "\
\Usage: dpi.hs [OPTIONS] WIDTH HEIGHT DIAG\n\
\Calculate DPI (dots per inch) from screen resolution and screen size.\n\
\\n\
\  WIDTH and HEIGHT: in pixels\n\
\  DIAG: measurement of screen in inches\n\
\\n\
\Options:\n\
\  -h, --help  This usage information"


main :: IO ()
main = do
   -- Get list of string args from the env
   argStrings <- getArgs

   -- Examine the number of arguments given
   case (length argStrings) of
      3 -> do
         -- Convert to numeric type and bind them
         let (width : height : diag : []) = map read argStrings

         -- Calculate dpi from the input
         let dpi = (sqrt ((width ^ 2) + (height ^ 2))) / diag

         -- Display results
         printf "   resolution (pixels, W x H): %.0f x %.0f\n"
            (width::Float) (height::Float)
         printf "diagonal screen size (inches): %.1f\n" (diag::Float)
         printf "          dots per inch (DPI): %.2f\n" (dpi::Float)

      otherwise -> displayUsage
