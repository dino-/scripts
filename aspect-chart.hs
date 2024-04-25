#! /usr/bin/env stack
{- stack runghc -}

import Data.List (sort)
import Data.Map (Map (..), (!), fromList, toList)
import Text.Printf (printf)


-- Got these from [Wikipedia](https://en.wikipedia.org/wiki/Display_aspect_ratio)
aspects :: Map Double String
aspects = fromList
  [ (1.33, "4:3")
  , (1.25, "5:4")
  , (1.50, "3:2")
  , (1.60, "16:10")
  , (1.77, "16:9")
  , (2.33, "21:9")
  , (3.55, "32:9")
  ]


-- Got these from xrandr output with some hand-formatting
resolutions :: [(Int, Int)]
resolutions =
  [ (3440, 1440)
  , (2560, 1600)
  , (2560, 1080)
  , (1920, 1080)
  , (1680, 1050)
  , (1600,  900)
  , (1280, 1024)
  , (1280,  800)
  , (1152,  864)
  , (1280,  720)
  , (1024,  768)
  , ( 800,  600)
  , ( 720,  576)
  , ( 720,  480)
  , ( 640,  480)
  ]


data AspectRatio = AspectRatio
  { arWidth :: Int
  , arHeight :: Int
  , arRatio :: Double
  , arHumanReadable :: String
  }


-- Compute the nearest well-known aspect ratio given the actual one for a given
-- resolution. Return the human-readable string for it.
humanReadable :: Double -> String
humanReadable actualRatio = (!) aspects . snd . head $ sortedByDistance
  where
    -- This is just all the keys from aspects
    knownRatios = map fst $ toList aspects

    -- SortedByDistance is a sorted
    --   [(distance from actual ratio to known ratio, known ratio)]
    sortedByDistance = sort $ zip
      (map (\knownRatio -> abs (knownRatio - actualRatio)) knownRatios)
      knownRatios


-- Compute an AspectRatio data structure given a width and height of a display
mkAspectRatio :: (Int, Int) -> AspectRatio
mkAspectRatio (w, h) = AspectRatio w h ratio (humanReadable ratio)
  where
    ratio = (fromIntegral w) / (fromIntegral h)


main :: IO ()
main = do
  let as = map mkAspectRatio resolutions
  putStrLn "width  height  aspect ratio"
  putStrLn "---------------------------"
  mapM_ (\a -> printf "%4d x %4d    %.2f  %s\n"
    (arWidth a) (arHeight a) (arRatio a) (arHumanReadable a)) as
