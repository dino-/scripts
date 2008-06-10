#! /usr/bin/env runhaskell

import Control.Monad (replicateM, replicateM_)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import System.Random
import Text.Printf
import Text.Regex (mkRegex, matchRegex)


type Dicemap = Map.Map String String


dicewareWordlistPath = "diceware.wordlist.asc"

defaultNumWords = "6"
defaultNumLines = "20"


{- Load the diceware word list file into a Map
-}
loadWordlist :: IO Dicemap
loadWordlist = do
   contents <- readFile dicewareWordlistPath
   let wordlistLines = lines contents
   let parsed =
         map (\(k:v:[]) -> (k, v))
         $ catMaybes
         $ map (matchRegex $ mkRegex "([0-9]{5})\t(.*)") wordlistLines
   return $ Map.fromList parsed


{- Randomly generate dice rolls and return the corresponding diceware
   word
-}
getWord :: Dicemap -> IO String
getWord dm = do
   g <- newStdGen
   let rNums = take 5 $ randomRs (1, 6 :: Int) g
   let key = concat $ map show rNums
   Map.lookup key dm


{- Pick lists of lists of diceware words given a number of words per line
   and a number of lines
-}
pickWords :: [String] -> IO ()

pickWords []                         =
   pickWords [defaultNumWords, defaultNumLines]

pickWords (numWords : [])            =
   pickWords [numWords, defaultNumLines]

pickWords (numWords : numLines : []) = do
   mapWordlist <- loadWordlist

   replicateM_ (read numLines) $ do
      -- Generate the words
      wordsLine <- replicateM (read numWords) $ getWord mapWordlist
      -- Calculate the length of the words only
      let nospaceLength = sum $ map length wordsLine
      -- Format a display string with interspersed spaces
      let displayString = intercalate " " wordsLine

      printf "%-54s | words: %d  chars: %d\n"
         displayString nospaceLength (length displayString)


main :: IO ()
main = getArgs >>= pickWords
