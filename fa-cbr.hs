#! /usr/bin/env runhaskell

{- This script will take the number of an episode of Warren Ellis
   and Paul Duffield's FreakAngels weekly online comic and bundle 
   the pages into a Comic Book Archive file on your local system.

   At the time of last editing of this script, the Freakangels page:
     http://www.freakangels.com/

   Written by adoring fan Dino Morelli <dino@ui3.info>
   2010-10-24

   This is, of course, free software. Give it to your friends. Enjoy
-}

import Control.Monad
import Control.Monad.Error
import Data.Maybe
import System.Cmd
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import Text.Printf
import Text.Regex


outputProgInfo :: IO ()
outputProgInfo = do
   progName <- getProgName
   putStrLn $ progName ++
      "  v2.3.1  Dino Morelli <dino@ui3.info>"
   putStrLn "http://ui3.info/darcs/scripts/fa-cbr.hs\n"


data EP = EP String (IO ExitCode)

instance Error EP where
   strMsg msg = EP msg $ return ExitSuccess


getEpisode :: [String] -> ErrorT EP IO String

getEpisode (e:_) = do
   return $ printf "%04d" $ (read e :: Int)

getEpisode _     = do
   files <- liftIO $ getDirectoryContents "."
   let re = mkRegex "([0-9]+)"
   let nss = map head . catMaybes .
         map (matchRegex re) $ files
   let m = foldl max (-1) . map read $ nss :: Int

   when (m < 0) $ throwError $
      EP "Unable to determine next issue number"
         (return $ ExitFailure 1)

   return $ printf "%04d" (m + 1)


checkFileExists :: FilePath -> ErrorT EP IO ()
checkFileExists cbtFile = do
   exists <- liftIO $ doesFileExist cbtFile
   when exists $ throwError $ EP (printf "CBR file %s already exists. Aborting!" cbtFile)
      (return $ ExitFailure 2)


cleanUp :: MonadIO m => FilePath -> m ()
cleanUp dirPath = liftIO $ do
   putStrLn "Cleaning up.."
   removeDirectoryRecursive dirPath


downloadFiles :: String -> FilePath -> ErrorT EP IO ()
downloadFiles episode dirPath = do
   -- Build some strings we'll need
   let baseUrl = "http://www.freakangels.com"
   let imgPrefix = printf "%s/comics/FA%s-" baseUrl episode :: String
   let imgSuffix = ".jpg"

   cwd <- liftIO $ do
      putStrLn "Creating dir for downloaded images.."
      createDirectory dirPath
      getCurrentDirectory  -- Save this path for later

   -- Try first image to check if this is a valid issue
   dlSucceeded <- liftIO $ do
      setCurrentDirectory dirPath
      putStrLn "Downloading images.."
      system $ printf "wget %s%d%s" imgPrefix (1 :: Int) imgSuffix
      ds <- fmap not $ doesFileExist "index.html"
      setCurrentDirectory cwd
      return ds

   -- It's not a valid issue, stop now
   unless dlSucceeded $ throwError $ EP (printf "Pages for episode %s were not found. Is this a valid episode number?" episode)
      (cleanUp dirPath >> (return $ ExitFailure 3))

   -- We're good, get the rest of the pages
   liftIO $ do
      setCurrentDirectory dirPath
      forM_ [2..6] $ \n ->
         system $ printf "wget %s%d%s" imgPrefix (n :: Int) imgSuffix
      setCurrentDirectory cwd


constructCbr :: FilePath -> FilePath -> ErrorT EP IO ()
constructCbr cbtFile dirPath = do
   liftIO $ putStrLn "Constructing CBR file.."
   liftIO $ system $ printf "tar cvf %s %s" cbtFile dirPath

   cleanUp dirPath

   liftIO $ putStrLn $ printf "Complete. Your new file: %s" cbtFile


main :: IO ()
main = do
   outputProgInfo

   result <- runErrorT $ do
      episode <- liftIO getArgs >>= getEpisode

      let dirPath = printf "FreakAngels_%s" episode
      let cbtFile = dirPath ++ ".cbt"

      checkFileExists cbtFile

      downloadFiles episode dirPath

      constructCbr cbtFile dirPath

      return ExitSuccess

   either (\(EP msg a) -> do
      putStrLn $ "ERROR: " ++ msg
      a >>= exitWith
      )
      exitWith result
