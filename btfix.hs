#! /usr/bin/env stack
{- stack runghc -}

{- No stack? Use this #! instead of the two lines above:
#! /usr/bin/env runhaskell
-}

{-
  Script to disconnect my most-commonly-used bluetooth headphone, make sure the
  bluetooth hardware is powered in and reconnect the headphone.

  Dino Morelli <dino@ui3.info>
  v1.0  2021-09-05
-}

import Data.List
import Data.Maybe
import System.Exit
import System.Process


-- Set headphoneDevice to your most commonly-used headphone device as reported
-- by `bluetoothctl devices`

headphoneDevice :: String
headphoneDevice = "00:1B:66:BC:65:71"

main :: IO ()
main = exitWith =<< ( sequenceWorstEC
  [ system $ "bluetoothctl disconnect " <> headphoneDevice
  , powerBluetooth
  , system $ "bluetoothctl connect " <> headphoneDevice
  ] )

{- This is similar to the for/in/do/done construct in bash with an important
   difference. This action evaluates to the "worst" exit code of all IO actions
   that are passed after they are evaluated.
-}
sequenceWorstEC :: [IO ExitCode] -> IO ExitCode
sequenceWorstEC as =
  fromMaybe ExitSuccess . listToMaybe . reverse . sort <$> sequence as

powerBluetooth :: IO ExitCode
powerBluetooth = do
  isPowered <- isBluetoothPowered
  if isPowered
    then do
      putStrLn "Bluetooth is already powered on"
      return ExitSuccess
    else system "bluetoothctl power on"

isBluetoothPowered :: IO Bool
isBluetoothPowered =
  ( all (== False)
  . map (isInfixOf " no")
  . filter (isInfixOf "Powered")
  . lines
  ) <$> readProcess "bluetoothctl" ["show"] ""
