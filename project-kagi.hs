#! /usr/bin/env stack
{- stack --resolver lts-20.12 runghc --package mtl --package safe -}
-- NOTE: lts-20.12 uses GHC 9.2.x which we need for overloaded record dot

{-# LANGUAGE OverloadedRecordDot #-}

-- If you need a specific resolver, do this:
-- stack --resolver lts-7.8 runghc

{- No stack? Use this #! instead of the two lines above:
#! /usr/bin/env runhaskell
-}

import Control.Monad.Except (MonadError, runExcept, throwError)
import Data.Time (Day, addDays, diffDays)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.LocalTime (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime),
  getZonedTime)
import Safe (readMay)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import Text.Printf (printf)


dayFormat :: String
dayFormat = "%Y-%m-%d"


parseTime :: MonadError String m => String -> m Day
parseTime timeString =
  maybe (throwError $ "Can't parse date from '" <> timeString <> "'") pure
    $ parseTimeM False defaultTimeLocale dayFormat timeString


parseArgs :: [String] -> Maybe String -> Either String (Day, Integer)

parseArgs (searchCountS : subStartS : []) _ = runExcept $ do
  subStart <- parseTime subStartS
  searchCount <- maybe (throwError $ "bad search count") pure $ readMay searchCountS
  pure (subStart, searchCount)

parseArgs (searchCountS : _) (Just subStartS) =
  parseArgs [searchCountS, subStartS] Nothing

parseArgs (searchCountS : []) Nothing =
  runExcept . throwError $ "No sub start date supplied"

parseArgs _ _ = runExcept . throwError $ "must supply a search count"


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


subStartEnvName = "KAGI_SUB_START"


main :: IO ()
main = do
  eParsedArgs <- parseArgs <$> getArgs <*> lookupEnv subStartEnvName
  either handleFailure handleSuccess eParsedArgs


handleFailure :: String -> IO ()
handleFailure errMsg = do
  printf "ERROR %s\n" errMsg
  printf "  [%s=\"SUB_START_DATE\"] project-kagi.hs SEARCHES_USED [SUB_START_DATE]\n\n" subStartEnvName
  printf "  %s=\"2025-04-11\" project-kagi.hs 42\n" subStartEnvName
  putStrLn "  project-kagi.hs 42 2025-04-11"
  exitFailure


handleSuccess :: (Day, Integer) -> IO ()
handleSuccess parsedArgs = do
  today' <- localDay . zonedTimeToLocalTime <$> getZonedTime
  let (currentUsage, projectedUsage) = calculateUsage today' parsedArgs
  displayCurrent currentUsage
  displayProjection projectedUsage
