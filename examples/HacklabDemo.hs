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
  (inputKerho,_) <- poll $ readInputBits master 1 0 8
  (inputPaja,_)  <- poll $ readInputBits master 2 0 8
  -- Our switch variables, initially all OFF
  kerhoValot    <- newTVarIO False
  kerhoLaitteet <- newTVarIO False
  pajaValot     <- newTVarIO False
  pajaLaitteet  <- newTVarIO False
  -- Connect variables to given relays
  relayControl (readTVar kerhoValot)    (writeBit master 1 0)
  relayControl (readTVar kerhoLaitteet) (writeBit master 1 1)
  relayControl (readTVar pajaValot)     (writeBit master 1 2)
  relayControl (readTVar pajaLaitteet)  (writeBit master 1 3)
  -- Toggle states with Modbus input
  toggleButton True  (inputKerho `item` 0) kerhoValot
  toggleButton True  (inputKerho `item` 1) kerhoLaitteet
  toggleButton False (inputPaja  `item` 0) pajaValot
  toggleButton True  (inputPaja  `item` 1) pajaLaitteet
  -- Introduce a panic switch which turns off everything
  pushButton (inputKerho `item` 3) nop $ atomically $ do
    writeTVar kerhoValot    False
    writeTVar kerhoLaitteet False
    writeTVar pajaValot     False
    writeTVar pajaLaitteet  False
  -- Report when club evening starts and ends
  pushButton (inputKerho `item` 2)
    (putStrLn "Kerhoilta loppui")
    (putStrLn "Kerhoilta alkoi")
  putStrLn "Up and running"
  atomically $ waitFailure $ getStats master
