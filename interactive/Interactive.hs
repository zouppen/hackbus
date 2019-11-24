-- |Quick hack for getting interactive prompt
module Interactive where

import System.Hardware.Modbus
import System.Hardware.Modbus.LowLevel
import qualified System.Console.Readline as R

modbusInit = do
  R.setCompletionEntryFunction (Just R.filenameCompletionFunction)
  ans <- R.readline "Device file: "
  device <- case ans of
    Nothing -> fail "Interrupted"
    Just x -> return $ takeWhile (/=' ') x
  h <- newRTU device 9600 ParityNone 8 1
  connect h
  return h
