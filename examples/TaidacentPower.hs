-- |Loop and collect measurements from the Taidacent energy meter
-- https://www.aliexpress.com/item/4001192198196.html
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Control.Concurrent
import System.Environment
import System.Hardware.Modbus.LowLevel
import Control.Exception
import Control.Monad.Loops

tries = 3

main = do
  [device] <- getArgs
  h <- newRTU device 1200 ParityNone 8 1
  connect h
  setSlave h 1
  flip (iterateUntilM (==0)) tries $ \n -> do
    stuff <- try $ readRegisters h 1 2
    case stuff of
      Left (_ :: ModbusException) -> pure $ n-1 -- Retry
      Right [voltage,current] -> do
        putStrLn $ "voltage=" ++ (show voltage) ++ "\ncurrent=" ++ (show current) 
        pure 0 -- Stop after successful read
  close h
