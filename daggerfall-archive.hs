#! /usr/bin/env runhaskell

import Control.Monad
import System.Cmd
import System.Directory
import System.Environment
import System.Exit
import System.Locale
import System.Time
import Text.Printf


main :: IO ()
main = do
   fmtMask <- fmap (printf
      "%s/savegame/daggerfall-archive/daggerfall-%%Y%%m%%d.tar.gz")
      $ getEnv "HOME"

   archivePath <- fmap (formatCalendarTime defaultTimeLocale
      fmtMask) (getClockTime >>= toCalendarTime)

   doesFileExist archivePath >>= \e -> when e $ do
      putStrLn $ printf "Error, file %s exists" archivePath
      exitFailure

   setCurrentDirectory "/var/local/dosbox/DAGGER"
   exitCode <- system $ printf "tar czf %s SAVE*" archivePath

   when (ok exitCode) $
      putStrLn $ printf "\nDaggerfall savegames backed up to %s" archivePath

   exitWith exitCode


ok :: ExitCode -> Bool
ok ExitSuccess = True
ok _           = False
