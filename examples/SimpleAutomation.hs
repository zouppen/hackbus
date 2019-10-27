module Main where

import Control.Concurrent.STM
import System.Environment (getArgs)
import System.Hardware.Modbus
import System.Hardware.Modbus.Abstractions

main = do
  -- Minimal command line parsing
  args <- getArgs
  device <- case args of
    [a] -> return a
    _   -> fail "Give serial device name"
  -- Start Modbus master
  h <- newRTU device 9600 ParityNone 8 1
  connect h
  master <- runMaster h
  -- Get input banks from both rooms
  ([kytkinKerhoValot, kytkinKerhoLaitteet, kytkinKerhoilta, kytkinPaniikki],_) <-
    pollMany $ readInputBits master 1 0 4
  ([kytkinPajaValot, kytkinPajaLaitteet],_) <-
    pollMany $ readInputBits master 2 0 2
  -- Our switch variables, initially all OFF
  kerhoValot    <- newTVarIO False
  kerhoLaitteet <- newTVarIO False
  pajaValot     <- newTVarIO False
  pajaLaitteet  <- newTVarIO False
  -- Connect variables to given relays
  wire (readTVar kerhoValot)    (writeBit master 1 0)
  wire (readTVar kerhoLaitteet) (writeBit master 1 1)
  wire (readTVar pajaValot)     (writeBit master 1 2)
  wire (readTVar pajaLaitteet)  (writeBit master 1 3)
  -- Toggle states with Modbus input
  toggleButton True  kytkinKerhoValot    kerhoValot
  toggleButton True  kytkinKerhoLaitteet kerhoLaitteet
  toggleButton False kytkinPajaValot     pajaValot
  toggleButton True  kytkinPajaLaitteet  pajaLaitteet
  -- Introduce a panic switch which turns off everything
  pushButton kytkinPaniikki nop $ atomically $ do
    writeTVar kerhoValot    False
    writeTVar kerhoLaitteet False
    writeTVar pajaValot     False
    writeTVar pajaLaitteet  False
  -- Report when club evening starts and ends
  pushButton kytkinKerhoilta
    (putStrLn "Kerhoilta loppui")
    (putStrLn "Kerhoilta alkoi")
  putStrLn "Up and running"
  atomically $ waitFailure $ getStats master
