#! /usr/bin/env runhaskell

{- Script to convert my old password file (plain text encrypted
with GPG) into KeePass 1.x XML format.

This was for one-time import.
-}

import Control.Monad
import Data.Time
import Data.Time.Format
import System.Locale
import System.Process
import System.Time
import Text.ParserCombinators.Parsec
import Text.Printf


main :: IO ()
main = do
   -- Call old password database tool to decrypt my password db,
   -- capturing the results
   oldPws <- readProcess "pd" ["-v"] ""

   -- Parse that data into a list of PwEntry data structures
   let result = parsePwEntries "pwfile" oldPws

   -- Print out any error or make and print the XML
   either print outputXml result


{- My existing password file format

---- file start ----
Desc: foo user1
Username: user1
Password: blah
Url: 
Date: 2009-07-28
Notes: credentials for user1 on foo
   This server does something special
--------------
Desc: bar.com
Username: someuser
Password: somepw
Url: http://bar.com
Date: 2010-03-24
Notes: blah blah
--------------
--------------
---- file end ----
-}


data PwEntry = PwEntry     -- KeePass XML elements
   { desc :: String        -- title
   , username :: String    -- username
   , password :: String    -- password
   , url :: String         -- url
   , date :: String        -- creation, lastmod
   , notes :: String       -- comment
   }
   deriving Show


parsePwEntries :: SourceName -> String -> Either ParseError [PwEntry]
parsePwEntries name = runParser pPwEntries () name


pPwEntries :: Parser [PwEntry]
pPwEntries = do
   bs <- many pPwEntry
   string "--------------"
   return bs


pPwEntry :: Parser PwEntry
pPwEntry = do
   d <- pField "Desc"
   n <- pField "Username"
   p <- pField "Password"
   u <- pField "Url"
   m <- pField "Date"
   c <- pNotes
   return $ PwEntry d n p u (formatDate m) c


pField :: String -> Parser String
pField label = do
   string label
   string ": "
   manyTill anyChar eol


pNotes :: Parser String
pNotes = do
   string "Notes:"
   optional $ char ' '
   manyTill anyChar (try (string "--------------\n"))


many1Till :: Parser a -> Parser end -> Parser [a]
many1Till p end = do
   h <- p 
   t <- manyTill p end
   return $ h : t


eol :: Parser Char
eol = newline <|> (eof >> return '\n')


formatDate :: String -> String
formatDate rawDate = maybe "" formatDate' parseDate
   where
      formatDate' = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
      parseDate = foldl mplus Nothing $ map ($ rawDate)
         [ parseDateString "%-m/%-d/%y"
         , parseDateString "%Y-%m-%d"
         ]


parseDateString :: String -> String -> Maybe UTCTime
parseDateString = parseTime defaultTimeLocale


{- KeePass 1.x XML format:

<!DOCTYPE KEEPASSX_DATABASE>

<database>
   <group>
      <title>Internet</title>
      <icon>1</icon>

      <entry>
         <title>this is the title</title>
         <username>username here</username>
         <password>passwordhere</password>
         <url>url here</url>
         <comment>comment here</comment>
         <icon>0</icon>
         <creation>2009-09-28T17:00:10</creation>
         <lastaccess>2009-09-28T17:00:38</lastaccess>
         <lastmod>2009-09-28T17:00:38</lastmod>
         <expire>Never</expire>
      </entry>
   </group>
   <group>
      <title>eMail</title>
      <icon>19</icon>
   </group>
</database>
-}

outputXml :: [PwEntry] -> IO ()
outputXml pes = do
   putStrLn "<!DOCTYPE KEEPASSX_DATABASE>\n"
   putStrLn "<database>"
   putStrLn "   <group>"
   putStrLn "      <title>Old</title>"
   putStrLn "      <icon>1</icon>"
   mapM_ outputPwEntry pes
   putStrLn "   </group>"
   putStrLn "</database>"


outputPwEntry :: PwEntry -> IO ()
outputPwEntry pe = do
   putStrLn "      <entry>"
   printf   "         <title><![CDATA[%s]]></title>\n" (desc pe)
   printf   "         <username><![CDATA[%s]]></username>\n" (username pe)
   printf   "         <password><![CDATA[%s]]></password>\n" (password pe)
   printf   "         <url><![CDATA[%s]]></url>\n" (url pe)
   printf   "         <creation>%s</creation>\n" (date pe)
   printf   "         <lastmod>%s</lastmod>\n" (date pe)
   printf   "         <comment><![CDATA[%s]]></comment>\n" (notes pe)
   putStrLn "         <expire>Never</expire>"
   putStrLn "      </entry>"


-- These are for debugging

printDescPw :: PwEntry -> IO ()
printDescPw pe = printf "%-37s %s\n" (desc pe) (password pe)

printDateDesc :: PwEntry -> IO ()
printDateDesc pe = printf "%s  %s\n" (date pe) (desc pe)
