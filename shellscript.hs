#! /usr/bin/env runhaskell

--import Control.Monad ( when, unless )
--import System.Cmd
import System.Directory
--import System.Environment
import System.Exit
import System.Locale
--import System.Process
import System.Time
import Text.Printf


main :: IO ()
main = do
   printf "This is a shell script\n"

   putStrLn =<< date

   home <- getHomeDirectory
   let prompt = printf "HOME is %s" home
   putStrLn prompt

   logM "Example of a log message"


{- Get the current date/time as a string in RFC822 format
   Looks like this in my locale: Mon Feb 13 16:21:38 EST 2012
-}
date :: IO String
date = dateFormat "%c"


{- Get the current date/time as a string in the specified format
   For format string help, see man 3 strftime
-}
dateFormat :: String -> IO String
dateFormat fmt = fmap (formatCalendarTime defaultTimeLocale
   fmt) (getClockTime >>= toCalendarTime)


{- Output a message with datestamp
-}
logM :: String -> IO ()
logM msg = do
   date <- dateFormat "%Y-%m-%d %H:%M:%S"
   printf "%s> %s\n" date msg


{- Turn an exit code (say, from system) into a Bool
-}
ok :: ExitCode -> Bool
ok ExitSuccess = True
ok _           = False


{- This is a skeleton file for writing shell scripts with Haskell
   Use what you need from this and delete the rest, including 
   unused imports.

   Some common bash things to use here:

   bash                             Haskell
   ----                             -------
   dates                            System.Locale, System.Time
      date                             date

      date +"%Y%m%d"                   dateFormat "%Y%m%d"
         result: 20120213

   file/dir things                  System.Directory
      $HOME                            getHomeDirectory
      [ -f FILE ]                      doesFileExist FILE
      [ -d DIR ]                       doesDirectoryExist DIR
      pwd                              getCurrentDirectory
      cd DIR                           setCurrentDirectory DIR

   other env variables              System.Environment
      $VAR                             getEnv "VAR"

   string interpolation             Text.Printf
      "foo $bar ${baz}"                printf "foo %s %d" bar baz

   execution, exit code             System.Cmd
      program -x val arg               ec <- system "program -x val arg"
      ec=$?

   execution, capture stdout        System.Process
      output=$(program -x val arg)     output <- readProcess "program" ["-x", "val", "arg"] "stdin data, if desired"

   exiting                          System.Exit
      exit 0                           exitSuccess
      exit 1                           exitFailure
      exit INT                         exitWith $ ExitFailure INT

-}
