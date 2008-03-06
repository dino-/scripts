#! /usr/bin/env runhaskell

import System.Environment
import System.Posix


modes :: FileMode
modes = ownerModes       `unionFileModes`
        groupReadMode    `unionFileModes`
        groupExecuteMode `unionFileModes`
        otherReadMode    `unionFileModes`
        otherExecuteMode


makeScript :: [String] -> IO ()

makeScript [] = do
   progName <- getProgName
   putStrLn $ "usage: " ++ progName ++ " SCRIPT"

makeScript (scriptName:_) = do
   writeFile scriptName contents
   setFileMode scriptName modes
   where
      contents = unlines
         [ "#! /usr/bin/env runhaskell"
         , ""
         , "main = do"
         , "   putStrLn \"Your new script!\""
         ]


main = getArgs >>= makeScript
