#! /usr/bin/env runhaskell

-- Script to start a systemd service unit and tail its log at the same time

import Control.Arrow ((&&&))
import Control.Monad (void)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.Process (callProcess, spawnProcess)
import Text.Printf (printf)


main :: IO ()
main = getArgs >>= parseArgs


parseArgs :: [String] -> IO ()
parseArgs []                = usageAndExit $ ExitFailure 1
parseArgs allArgs
  | "-h"     `elem` allArgs = usageAndExit ExitSuccess
  | "--help" `elem` allArgs = usageAndExit ExitSuccess
  | otherwise               = startService $ (init &&& last) allArgs


usageAndExit :: ExitCode -> IO ()
usageAndExit ec = do
  pn <- getProgName
  putStrLn $ unlines
    [ printf "usage: %s [SYSTEMCTL-ARGS] SERVICE" pn
    , ""
    , printf "  %s some-system-service.service" pn
    , printf "  %s -S today some-system-service" pn
    , printf "  %s --user some-user-service" pn
    , ""
    , "v1.0  2025-01-19  Dino Morelli <dino@ui3.info>"
    ]
  exitWith ec


startService :: ([String], String) -> IO ()
startService (args, service) = do
  void $ spawnProcess "systemctl" $ args ++ ["start", service]
  callProcess "journalctl" $ args ++ ["-u", service, "-f"]
