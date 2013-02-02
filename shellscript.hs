#! /usr/bin/env runhaskell

{- This is a 'skeleton file' for writing shell scripts with Haskell

   Haskell is my go-to language for anything more complicated than a
   simple sequence of shell commands in a bash script. Doing list
   processing and a lot of logic in bash is a grind, to me.

   The idea here is to take a copy of this script and hack it to do
   what you need quickly. Throw the rest out.

   The script starts off (after the imports) with some simple examples
   in main.

   After that are a few functions that simplify things like getting
   the date as a String, logging a date-stamped String to stdout and
   manupulating an ExitCode as a true/false value.

   Finally, there is a comment block full of examples in bash and some
   Haskell code to do something along the same lines.

   version: 1.0
-}

--import Control.Monad ( when, unless )
--import System.Cmd
import System.Directory
--import System.Environment
import System.Exit
import System.Locale
--import System.Process
import System.Time
import Text.Printf
--import Text.Regex


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


{- Some common bash things and their Haskell counterparts:

   bash                             Haskell
   ----                             -------
   dates                            System.Locale, System.Time
      date                             date

      date +"%Y%m%d"                   dateFormat "%Y%m%d"
         result: 20120213
                                    (these two functions above)

   file/dir things                  System.Directory
      $HOME                            getHomeDirectory
      [ -f FILE ]                      doesFileExist FILE
      [ -d DIR ]                       doesDirectoryExist DIR
      pwd                              getCurrentDirectory
      cd DIR                           setCurrentDirectory DIR

   arguments                        System.Environment
      arg1=$1                          (arg1 : arg2 : _) <- getArgs
      arg2=$2

   environment variables            System.Environment
      $VAR                             getEnv "VAR"

   string interpolation             Text.Printf
      "foo $bar ${baz}"                printf "foo %s %d" bar baz

   execution, exit code             System.Cmd
      program -x val arg               ec <- system "program -x val arg"
      ec=$?

   execution, capture stdout        System.Process
      output=$(program -x val arg)     output <- readProcess "program" ["-x", "val", "arg"] "stdin data, if desired"

      or use System.Process.readProcessWithExitCode
      :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
         program     args        stdin         exitcode  stdout  stderr

   exiting                          System.Exit
      exit 0                           exitSuccess
      exit 1                           exitFailure
      exit INT                         exitWith $ ExitFailure INT

   regular expressions              Text.Regex
      (see bash docs for =~            mbMatches = matchRegex
       and BASH_REMATCH)                  (mkRegex "a(.)b(.)") string
                                          :: Maybe [String]

-}
