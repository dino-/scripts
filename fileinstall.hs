#! /usr/bin/env runhaskell

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Error
import Data.List (isInfixOf)
import System.Cmd (system)
import System.Directory (copyFile)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeFileName)
import Text.Printf (printf)


main :: IO ()
main = getArgs >>= parseArgs


parseArgs :: [String] -> IO ()
parseArgs []            = usage  -- No args at all
parseArgs args
   | length args < 3    = usage  -- Not enough args
   | elem "-h" args     = usage  -- User requested help
   | elem "--help" args = usage  -- User requested help
   | otherwise          = installFiles args  -- We're good, do it!


usage :: IO ()
usage = do
   appName <- getProgName
   putStrLn $ unlines
      [ "Mount a filesystem, copy files to a subdir, unmount it"
      , ""
      , "Usage: " ++ appName ++ " MOUNT_POINT DEST_DIR FILE1 [FILE2 ...]"
      , "       " ++ appName ++ " [OPTIONS]"
      , ""
      , "Options:"
      , "  -h, --help  This usage information"
      , ""
      , "This script will quietly overwrite files on the destination. Be careful!"
      , "If the filesystem was already mounted, it will be left that way."
      , ""
      , "Dino Morelli <dino@ui3.info>"
      ]


installFiles :: [String] -> IO ()
installFiles (mountPoint : destSuffix : files) = do
   result <- runErrorT $ do
      liftIO $ putStrLn ""

      -- mount the filesystem
      alreadyMounted <- mount mountPoint

      -- copy the files
      mapM_ (chattyCopy (mountPoint </> destSuffix)) files

      -- umount the filesystem
      if (alreadyMounted)
         then liftIO $ putStrLn "WARNING: filesystem was not unmounted!"
         else do
            liftIO $ putStrLn "Unmounting filesystem now.. please wait.."
            systemE $ "fusermount -u " ++ mountPoint
            liftIO $ putStrLn "Done"

      return ()

   either putStrLn return result


{- Mount the filesystem only if it's not already mounted. Returns
   True if it was already mounted, otherwise False
-}
mount :: (MonadIO m, MonadError String m) => String -> m Bool
mount mountPoint = do
   output <- liftIO $ readFile "/etc/mtab"

   if (mountPoint `isInfixOf` output)
      then do
         liftIO $ putStrLn "Filesystem already mounted"
         return True
      else do
         liftIO $ putStrLn "Filesystem not mounted, mounting now"
         systemE $ "mount " ++ mountPoint
         return False


{- Wrapper so that failed system commands produce failure in ErrorT
-}
systemE cmd = do
   ec <- liftIO $ system cmd
   case ec of
      ExitSuccess   -> return 0
      ExitFailure c -> throwError . show $ c


chattyCopy destDir srcPath = liftIO $ do
   let destPath = destDir </> takeFileName srcPath
   copyFile srcPath destPath
   printf "%s -> %s\n" srcPath destPath
