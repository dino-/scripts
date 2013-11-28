#! /usr/bin/env runhaskell

import System.Cmd (system)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath
import Text.Printf (printf)


priority, todoBinaryPath :: String
priority = "Z"
todoBinaryPath = "todo.sh"


main :: IO ()
main = getArgs >>= parseArgs


parseArgs :: [String] -> IO ()
parseArgs []            = usage  -- No args at all
parseArgs args
   | elem "-h" args     = usage  -- User requested help
   | elem "--help" args = usage  -- User requested help
   | otherwise          = importFile $ head args  -- We're good, do it!


usage :: IO ()
usage = do
   appName <- getProgName
   putStrLn $ unlines
      [ "Import items from a text file into todo.txt"
      , ""
      , "Usage: " ++ appName ++ " FILE"
      , "       " ++ appName ++ " [OPTIONS]"
      , ""
      , "Options:"
      , "  -h, --help  This usage information"
      , ""
      , "The items must be on one line each and prefixed with \"- \""
      , printf "They will be added to your todo list with a priority of %s"
         priority
      , ""
      , "Dino Morelli <dino@ui3.info>"
      ]


importFile :: FilePath -> IO ()
importFile textfilePath = do
   -- Read the file in and convert to a list of lines
   rawLines <- lines `fmap` readFile textfilePath

   -- Filter for just the todo items and strip the "- " off
   let todoLines = map (drop 2) .
         filter (\s -> take 2 s == "- ") $ rawLines

   -- Build a list of commands
   let commands = map mkCommand todoLines

   -- Execute the commands and exit with the status.
   -- If any failed, exit with failure.
   exitWith =<< forSystem commands


mkCommand :: String -> String
mkCommand td = printf "%s a \"(%s) %s\"" todoBinaryPath priority td


{- This is similar to the for/in/do/done construct in bash with an
   important difference. This action evaluates to ExitFailure 1 if
   *any* of the command executions fails. In bash you only get the
   exit code of the last one.
-}
forSystem :: [String] -> IO ExitCode
forSystem cs = do
   ecs <- mapM system cs
   return $ case all (== ExitSuccess) ecs of
      True  -> ExitSuccess
      False -> ExitFailure 1
