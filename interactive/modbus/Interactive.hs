-- |Quick hack for getting interactive prompt
module Interactive where

import Data.List
import Data.Maybe
import System.Hardware.Modbus
import System.Hardware.Modbus.LowLevel
import qualified System.Console.Readline as R

baudrates = ["110", "300", "600", "1200", "2400", "4800", "9600", "14400", "19200", "38400", "57600", "115200", "128000", "256000"]

modbusInit = do
  R.setCompletionEntryFunction (Just R.filenameCompletionFunction)
  ans <- R.readline "Device file: "
  device <- case ans of
    Nothing -> fail "Interrupted"
    Just x -> return $ takeWhile (/=' ') x
  R.setCompletionEntryFunction $ Just $ \input -> pure $ filter (isPrefixOf input) baudrates
  ans <- R.readline "Baud rate: "
  h <- newRTU device (read $ fromJust ans) ParityNone 8 1
  connect h
  return h
