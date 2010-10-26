#! /usr/bin/env runhaskell

{- This script will take the number of an episode of Warren Ellis
   and Paul Duffield's FreakAngels weekly online comic and bundle 
   the pages into a Comic Book Archive file on your local system.

   At the time of last editing of this script, the Freakangels page:
     http://www.freakangels.com/

   Written by adoring fan Dino Morelli <dino@ui3.info>
   2008-02-22, 2010-10-24

   This is, of course, free software. Give it to your friends. Enjoy
-}

import Control.Monad
import Data.List
import Data.Maybe
import System.Cmd
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import Text.Printf
import Text.Regex


exitBadInput = do
   putStrLn $ unlines $
      [ "FAILED!"
      , ""
      , "Please supply an episode number like '0001' or '2'"
      , "Whatever floats your boat as long as it's a real episode"
      , ""
      , "Really testing the input for all manner of totally whack"
      , "non-numeric values is kind of a pain in the ass. Please just"
      , "pass a number, eh?"
      , ""
      ]
   exitWith $ ExitFailure 1


dlFailMessage = do
   putStrLn $ unlines $
      [ "FAILED!"
      , ""
      , "One or more images for this episode were not found!"
      , ""
      ]


cleanUp dirPath = do
   putStrLn "Cleaning up.."
   removeDirectoryRecursive dirPath


main :: IO ()
main = do
   -- Identify yourself
   progName <- getProgName
   putStrLn $ progName ++
      "  v2.3.0  2010-10-24  Dino Morelli <dino@ui3.info>"
   putStrLn "http://ui3.info/darcs/scripts/fa-cbr.hs\n"

   -- Check the incoming episode number argument
   eraw <- do
      as <- getArgs
      case as of
         (e:[]) -> return e
         _      -> exitBadInput
   let episode = printf "%04d" $ (read eraw :: Int)

   printf "Attempting to get issue %s\n" episode

   -- Build some strings we'll need
   let baseUrl = "http://www.freakangels.com"
   let imgPrefix = printf "%s/comics/FA%s-" baseUrl episode :: String
   let imgSuffix = ".jpg"
   let dirPath = printf "FreakAngels_%s" episode

   putStrLn "Creating dir for downloaded images.."
   createDirectory dirPath
   cwd <- getCurrentDirectory  -- Save this path for later
   setCurrentDirectory dirPath

   putStrLn "Downloading images.."
   -- Let's hope the assumption of 6 pages per episode holds true
   forM_ [1..6] $ \n ->
      system $ printf "wget %s%d%s" imgPrefix (n :: Int) imgSuffix

   dlSucceeded <- fmap not $ doesFileExist "index.html"

   setCurrentDirectory cwd

   let cbtFile = dirPath ++ ".cbt"
   case dlSucceeded of
      True  -> do
         putStrLn "Constructing CBR file.."
         system $ printf "tar cvf %s %s" cbtFile dirPath

         cleanUp dirPath

         putStrLn $ printf "Complete. Your new file: %s" cbtFile

      False -> do
         dlFailMessage

         cleanUp dirPath

         exitWith $ ExitFailure 2
