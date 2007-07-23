#! /usr/bin/runhaskell

module Main where

import Control.Monad
import System.Console.GetOpt
import System.Environment ( getArgs )
import System.Posix.Env ( putEnv )
import System.Process ( runCommand )


data Flag
   = NoAction
   | SuppressStartup
   | Help
   deriving Eq


options :: [OptDescr Flag]
options =
   [
     Option ['n'] ["no-action"] (NoArg NoAction) 
         "Display what would be done, but do nothing"
   , Option ['s'] ["suppress"] (NoArg SuppressStartup) 
         "Suppress start-up programs"
   , Option ['h'] ["help"] (NoArg Help)
         "This help text"
   ]


parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv = 
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageText))


usageText :: String
usageText = (usageInfo header options) ++ "\n" ++ footer
   where
      header = init $ unlines
         [ "Usage: xgo.hs [OPTIONS] [LAYOUT]"
         , "Start the X server with specific options and layout"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "Valid layouts are:"
         , "  default      Use only external monitor if present, otherwise fall back on"
         , "               laptop LCD. Specifying no layout will use this one."
         , "  laptop       Force use of only laptop LCD."
         , "  both         Use both external and laptop LCD as a large Twinview desktop."
         , "  open-driver  Use open-source nv driver. This will use both video heads if"
         , "               present."
         , ""
         , "Version 001  2007-Jul-08  Dino Morelli <dino@ui3.info>"
         ]


-- Figure out and execute what the user wants based on the supplied args.
executeCommands :: ([Flag], [String]) -> IO ()

-- User requested help. Display it and that's it
executeCommands (flags, _)
   | (Help `elem` flags) = putStrLn usageText

-- Normal program operation.
executeCommands (flags, (layout:[])) = do
   when (SuppressStartup `elem` flags) $ do
      putStrLn "Start-up items suppressed."
      putEnv "XGOSUPPRESS=1"

   let commandString = "startx -- -layout " ++ layout

   -- Execute the command
   if (NoAction `elem` flags)
      then do
         putStrLn "No-action mode, X will not be started."
         putStrLn $ "command string: " ++ commandString
      else do
         --putStrLn $ "NOT YET!  " ++ commandString
         runCommand commandString
         return ()

-- No layout specified, use default
executeCommands (flags, []) = executeCommands (flags, ["default"])


main :: IO ()
main = do
   args <- getArgs
   parseOpts args >>= executeCommands
