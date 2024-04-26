#! /usr/bin/env stack
{- stack --resolver lts-20.12 runghc --package mtl --package safe -}
-- NOTE: lts-20.12 uses GHC 9.2.x which we need for overloaded record dot

{-# LANGUAGE OverloadedRecordDot #-}

-- If you need a specific resolver, do this:
-- stack --resolver lts-7.8 runghc

{- No stack? Use this #! instead of the two lines above:
#! /usr/bin/env runhaskell
-}

import Control.Monad.Except (runExcept, throwError)
import Data.Time (Day, addDays, diffDays)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.LocalTime (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime),
  getZonedTime)
import Safe (readMay)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)


dayFormat :: String
dayFormat = "%Y-%m-%d"


defaultSubStart :: String
defaultSubStart = "2024-04-11"


parseArgs :: [String] -> Either String (Day, Integer)

parseArgs (searchCountS : []) = parseArgs [searchCountS, defaultSubStart]

parseArgs (searchCountS : subStartS : []) = runExcept $ do
  subStart <- maybe (throwError $ "can't parse date") pure
    $ parseTimeM False defaultTimeLocale dayFormat subStartS
  searchCount <- maybe (throwError $ "bad search count") pure $ readMay searchCountS
  pure (subStart, searchCount)

parseArgs _ = runExcept . throwError $ "must supply a search count"


data CurrentUsage = CurrentUsage
  { subStartDate :: Day
  , today :: Day
  , maxSearches :: Integer
  , elapsedDays :: Integer
  , usedSearches :: Integer
  , searchesPerDay :: Double
  }

data ProjectedUsage
  = Over
      { projectedEndDay :: Day
      , daysShort :: Integer
      }
  | Under
      { projectedUsedSearches :: Integer
      , unusedSearches :: Integer
      }


standardSubscriptionQuota :: Integer
standardSubscriptionQuota = 3600


calculateUsage :: Day -> (Day, Integer) -> (CurrentUsage, ProjectedUsage)
calculateUsage today' (subStart, searchCount) = (currentUsage, projectedUsage)
  where
    elapsedDays = diffDays today' subStart
    projectedDays :: Integer = floor $ (fromIntegral elapsedDays)
      * (fromIntegral standardSubscriptionQuota) / (fromIntegral searchCount)
    projectedDay = addDays projectedDays subStart
    renewalDay = addDays 365 subStart
    differenceInDays = diffDays renewalDay projectedDay
    searchesPerDay = (fromIntegral searchCount) / (fromIntegral elapsedDays)
    projectedUsedSearches = floor $ searchesPerDay * (fromIntegral (diffDays renewalDay today'))
    unusedSearches = standardSubscriptionQuota - projectedUsedSearches

    currentUsage = CurrentUsage subStart today' standardSubscriptionQuota
      elapsedDays searchCount searchesPerDay
    projectedUsage = if differenceInDays > 0
      then Over projectedDay differenceInDays
      else Under projectedUsedSearches unusedSearches


formatTime' :: Day -> String
formatTime' = formatTime defaultTimeLocale dayFormat


displayCurrent :: CurrentUsage -> IO ()
displayCurrent currentUsage = do
  printf "Subscription started: %s  Today: %s  Elapsed days: %d\n"
    (formatTime' currentUsage.subStartDate)
    (formatTime' currentUsage.today)
    (currentUsage.elapsedDays)
  printf "Max searches: %d  Searches used: %d  Searches per day: %.1f\n"
    currentUsage.maxSearches currentUsage.usedSearches currentUsage.searchesPerDay


displayProjection :: ProjectedUsage -> IO ()

displayProjection (Over projectedEndDay' daysShort') = do
  putStrLn "\nUsage is projected to be over"
  let projectedDayStr = formatTime' projectedEndDay'
  printf "All searches will be used by %s\n" projectedDayStr
  printf "That is %d days before renewal\n" daysShort'

displayProjection (Under projectedUsedSearches' unusedSearches') = do
  putStrLn "\nUsage is projected to be under"
  printf "%d searches projected to be used by the renewal date\n" projectedUsedSearches'
  printf "%d unused searches during the subscription period\n" unusedSearches'


main :: IO ()
main = do
  eParsedArgs <- parseArgs <$> getArgs
  either handleFailure handleSuccess eParsedArgs


handleFailure :: String -> IO ()
handleFailure errMsg = do
  printf "ERROR %s\n" errMsg
  putStrLn "  project-kagi.hs SEARCHES_USED [SUB_START_DATE]"
  putStrLn "  project-kagi.hs 42 2011-04-11"
  printf "\n  Default SUB_START_DATE: %s\n" defaultSubStart
  exitFailure


handleSuccess :: (Day, Integer) -> IO ()
handleSuccess parsedArgs = do
  today' <- localDay . zonedTimeToLocalTime <$> getZonedTime
  let (currentUsage, projectedUsage) = calculateUsage today' parsedArgs
  displayCurrent currentUsage
  displayProjection projectedUsage
