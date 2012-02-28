#! /usr/bin/env runhaskell

{-
   To run this:
      $ cd NEWDIR
      $ size-for-web.hs OLDDIR/*.jpg


   This analysis was used to come up with the values in newSize below:
      oldsize     ratio       old      convert args   new
      1280x768    16:10 1.66  385k     --             385k
      2592x1456   16:9  1.78  1.61M    80%, 1920      501k
      3072x2304   4:3   1.33  3.627M   80%, 1600      390k
      3648x2736   4:3   1.33  5.126M   80%, 1600      544k
-}

import Data.Maybe
import System.Environment
import System.FilePath
import System.Process
import Text.Printf
import Text.Regex


main :: IO ()
main = do
   srcFiles <- getArgs
   sizes <- mapM getSize srcFiles
   let pairs = zip (map (buildCmd . newSize) sizes) srcFiles

   mapM_ exec pairs


{- Scrape output of identify with regexp to figure out the maximum 
   dimension of an image
-}
getSize :: String -> IO Int
getSize path = do
   output <- readProcess "identify" [path] ""
   let [w, h] = map read . fromJust
         . matchRegex (mkRegex " ([0-9]+)x([0-9]+) ") $ output
   return $ max w h


{- Compute a new max size based on the existing size. Note that 
   pictures that are 1280 are to be left alone, hence the Nothing
-}
newSize :: Int -> Maybe String
newSize 1280 = Nothing
newSize 2592 = Just "1920"
newSize 3072 = Just "1600"
newSize 3648 = Just "1600"
newSize size = error $ printf "ERROR unexpected image size: %d" size


{- Build a shell command using the new size
-}
buildCmd :: Maybe String -> String
buildCmd Nothing     = "cp"
buildCmd (Just size) = printf "convert -quality 80 -resize %sx%s" size size


exec :: (String, FilePath) -> IO ()
exec (cmdPrefix, srcPath) = do
   let base = takeBaseName srcPath
   let destPath = base ++ "-sw.jpg"
   let cmd = printf "%s %s %s" cmdPrefix srcPath destPath
   putStrLn cmd
   system cmd
   return ()
