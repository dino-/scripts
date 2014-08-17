#! /usr/bin/env runhaskell

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Error
import Data.List (intercalate, isInfixOf)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.Process (system)
import Text.Printf (printf)


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
   result <- runErrorT $ do
      -- mount the filesystem
      alreadyMounted <- mount mountPoint

      -- perform the shell command
      let command = intercalate " " commandParts
      systemP command

      -- debugging
      when alreadyMounted $
         liftIO $ printf "%s already mounted" mountPoint

      -- umount the filesystem
      unless alreadyMounted $ do
         _ <- systemE $ "fusermount -u " ++ mountPoint
         return ()

      return ()

   either (hPutStrLn stderr) return result


{- Mount the filesystem only if it's not already mounted. Returns
   True if it was already mounted, otherwise False
-}
mount :: (MonadIO m, MonadError String m) => String -> m Bool
mount mountPoint = do
   output <- liftIO $ readFile "/etc/mtab"

   if trimTrailingSlash mountPoint `isInfixOf` output
      then return True
      else do
         systemE $ "mount " ++ mountPoint
         return False


{- If the mount point has a trailing slash, it's a problem to find
   the string in /etc/mtab
-}
trimTrailingSlash :: String -> String
trimTrailingSlash s = if last s == '/'
   then init s
   else s


{- Wrapper so that failed system commands produce failure in ErrorT
-}
systemE :: (Num a, MonadIO m, MonadError String m) => String -> m a
systemE cmd = do
   ec <- liftIO $ system cmd
   case ec of
      ExitSuccess   -> return 0
      ExitFailure c -> throwError $ printf "command: %s\nexitcode: %d"
         cmd c


{- Wrapper so that failed system commands are chatty about the failure
   (but don't fail the monadic action they're in)
-}
systemP :: (MonadIO m, MonadError String m) => String -> m ()
systemP cmd = liftIO $ do
   ec <- system cmd
   case ec of
      ExitSuccess   -> return ()
      ExitFailure c -> printf "command: %s\nexitcode: %d" cmd c
