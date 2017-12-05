#! /usr/bin/env runhaskell

module Main where

import System.Environment
import Text.Printf


-- Display usage info for this script.
displayUsage :: IO ()
displayUsage = putStrLn $ unlines
   [ "Usage: entropybits.hs [OPTIONS] CHARS LENGTH"
   , "Calculate bits of entropy for a password/passphrase"
   , ""
   , "  CHARS: Number of values each element could be. In diceware, this would be how many distinct words."
   , "  LENGTH: Length of the password or passphrase"
   , ""
   , "These days (as of 2016-02) they're saying to shoot for 128 bits or higher. This is a pretty long alphanumeric password, around 22 characters, or 10 diceware words!"
   , ""
   , "Options:"
   , "  -h, --help  This usage information"
   , ""
   , "Two examples:"
   , ""
   , "A 14-character password made from alphanumerics, including upper/lower"
   , "  CHARS: A-Z (26), a-z (26), 0-9 (10) = 62"
   , "  LENGTH: 14"
   , "  entropybits.hs 62 14 = 83.359"
   , ""
   , "A 7-word passphrase made from the English diceware word list"
   , "  CHARS: 7789"
   , "  LENGTH: 7"
   , "  entropybits.hs 7789 7 = 90.491"
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
         -- Convert to numeric types and bind them
         let (ne : l : []) = map read argStrings

         -- Calculate entropy
         -- Note, when you don't have base 2 log, you can also do:
         --   (log ne) / (log 2)  Many calculators are like this.
         let entropy = (logBase 2 ne) * l

         -- Display results
         printf "   possible element values: %.0f\n" (ne :: Float)
         printf "password/passphrase length: %.0f\n" (l :: Float)
         printf "           bits of entropy: %.3f\n" (entropy :: Float)

      _ -> displayUsage
