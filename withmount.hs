#! /usr/bin/env runhaskell

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Except
import Data.List (isInfixOf)
import System.Environment (getArgs, getProgName)
import System.Exit ( ExitCode (..), exitFailure )
import System.IO (hPutStrLn, stderr)
import System.Process (system)
import Text.Printf (printf)

{-# ANN parseArgs "HLint: ignore Use infix" #-}


main :: IO ()
main = getArgs >>= parseArgs


parseArgs :: [String] -> IO ()
parseArgs []            = usage  -- No args at all
parseArgs args
   | length args < 2    = usage  -- Not enough args
   | elem "-h" args     = usage  -- User requested help
   | elem "--help" args = usage  -- User requested help
   | otherwise          = execute args  -- We're good, do it!


usage :: IO ()
usage = do
   appName <- getProgName
   putStrLn $ unlines
      [ "Mount a filesystem, perform the supplied shell command, unmount it"
      , ""
      , "Usage: " ++ appName ++ " MOUNT_POINT SHELL_COMMAND"
      , "       " ++ appName ++ " [OPTIONS]"
      , ""
      , "Options:"
      , "  -h, --help  This usage information"
      , ""
      , "If the filesystem was already mounted, it will be left that way."
      , ""
      , "Dino Morelli <dino@ui3.info>"
      ]


execute :: [String] -> IO ()
execute (mountPoint : commandParts) = do
   result <- runExceptT $ do
      -- mount the filesystem
      alreadyMounted <- mount mountPoint

      -- just being chatty
      when alreadyMounted $
         liftIO $ printf "%s already mounted" mountPoint

      -- perform the shell command, hold onto the result
      let command = unwords commandParts
      cmdEc <- liftIO $ system command

      -- umount the filesystem, whether the above command failed or not
      unless alreadyMounted $ umount mountPoint

      -- assess the command result so withmount can fail if it failed
      exitCodeThrow command cmdEc

   either (\errMsg -> hPutStrLn stderr errMsg >> exitFailure) return result


exitCodeThrow :: (MonadIO m, MonadError String m) => String -> ExitCode -> m ()
exitCodeThrow _        ExitSuccess           = return ()
exitCodeThrow command (ExitFailure exitCode) =
  throwError $ printf "command: %s\nexitcode: %d" command exitCode


systemThrow :: (MonadIO m, MonadError String m) => String -> m ()
systemThrow command = do
  ec <- liftIO $ system command
  exitCodeThrow command ec


{- Mount the filesystem only if it's not already mounted. Returns
   True if it was already mounted, otherwise False
-}
mount :: (MonadIO m, MonadError String m) => String -> m Bool
mount mountPoint = do
   output <- liftIO $ readFile "/etc/mtab"

   if trimTrailingSlash mountPoint `isInfixOf` output
      then return True
      else do
         systemThrow $ "mount " ++ mountPoint
         return False

{- If the mount point has a trailing slash, it's a problem to find
   the string in /etc/mtab
-}
trimTrailingSlash :: String -> String
trimTrailingSlash s = if last s == '/' then init s else s


umount :: (MonadIO m, MonadError String m) => String -> m ()
umount mountPoint = do
   systemThrow $ "fusermount -u " ++ mountPoint
   return ()
