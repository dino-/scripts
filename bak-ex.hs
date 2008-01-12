#! /usr/bin/runhaskell


import Data.List ( intersperse )
import System.Process ( runCommand, waitForProcess )
import System.Time


-- Trailing / on src dir means DON'T include the last dir from src
srcs =
   [ ("/boot", "")
   , ("/etc", "")
   , ("/home", "--exclude .mozilla/firefox/*/Cache --delete-excluded")
   , ("/root", "")
   , ("/var/lib/dpkg", "")
   , ("/var/log", "")
   , ("/var/mail", "")
   , ("/var/spool/cron", "")
   ]

commonSwitches = "-av -R --delete"

destPath = "user@host.blah:/top/level/remote/backup/dir"


-- This one redirects all errors to stdout, but otherwise lets the
-- invoker deal with logging it or not
output = "2>&1"

-- Some other possible output postfixes:
--output = "2>&1 | tee -a /var/tmp/bak-foo.log"
--output = "2>&1 > /var/log/bak-foo.log"


-- This is Haskell equivilent of the *nix date command
date :: IO String
date =
   getClockTime >>= toCalendarTime >>= return . calendarTimeToString


delimWithSpace :: [String] -> String
delimWithSpace = concat . intersperse " "


main = do
   let commands =
         map (\(srcPath, extraSwitches) -> delimWithSpace
            [ "rsync", commonSwitches, extraSwitches
            , srcPath, destPath, output
            ]
         ) srcs

   date >>= putStrLn

   -- Use this for a dry run
   mapM_ (\cmd -> putStrLn cmd)
   --mapM_ (\cmd -> putStrLn cmd >> runCommand cmd >>= waitForProcess)
      commands

   date >>= putStrLn
