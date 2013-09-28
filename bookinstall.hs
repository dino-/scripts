#! /usr/bin/env runhaskell

import Control.Monad.Error (liftIO, runErrorT, throwError)
import System.Cmd (system)
import System.Directory (copyFile)
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeFileName)
import Text.Printf (printf)


readers =
   -- mount point             directory where books are stored
   [ ("/media/mountpoint",    "dir/on/device")
   ]


main :: IO ()
main = getArgs >>= parseArgs


parseArgs :: [String] -> IO ()
parseArgs []            = usage
parseArgs args
   | elem "-h" args     = usage
   | elem "--help" args = usage
   | otherwise          =
      mapM_ (installToReader args) readers


usage :: IO ()
usage = putStrLn $ unlines
   [ "Usage: bookinstall.hs [OPTIONS] EPUB_FILES"
   , "Install epub book files on all connected readers"
   , ""
   , "Options:"
   , "  -h, --help  This usage information"
   , ""
   , "Dino Morelli <dino@ui3.info>"
   ]


installToReader :: [FilePath] -> (FilePath, FilePath) -> IO (Either String ())
installToReader epubFiles (mountPoint, destSuffix) =
   runErrorT $ do
      liftIO $ putStrLn ""

      -- mount the reader
      systemE $ "mount " ++ mountPoint
      liftIO $ printf "mounted %s\n" mountPoint

      -- copy the files
      mapM_ (chattyCopy (mountPoint </> destSuffix)) epubFiles

      -- umount the reader
      systemE $ "fusermount -u " ++ mountPoint
      liftIO $ printf "unmounted %s\n" mountPoint

      return ()


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
