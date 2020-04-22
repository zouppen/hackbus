{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Hackbus.Logging
import Control.Hackbus.UnixJsonInterface
import Control.Monad
import Data.Map.Lazy (fromList)
import System.Environment (getArgs)
import System.Hardware.Modbus
import System.Hardware.Modbus.Abstractions
import System.IO
import System.Process
import Media.Streaming.Vlc
import System.Hardware.Modbus.Types (Control)

import ActivityDetect

write4chRelay :: Master -> Int -> Int -> Control Bool
write4chRelay m s a v = writeRegister m s a (if v then 0x0100 else 0x0200)

delayOffSwitch var delay waitAct offAct onAct = flip (pushButton var) onAct $ do
  wait <- readTVar <$> registerDelay delay
  waitAct
  join . atomically $ do
    viive <- wait
    tila <- var
    case (viive, tila) of
      (True, False) -> return offAct      -- Viive kulunut, katkaisin yhä alhaalla
      (_, True)     -> return $ return () -- Katkaisin ylhäällä, peruuta
      _             -> retry              -- Muuten odotellaan

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

  -- Audio source
  vlcH <- vlcStart (Just "/home/joell") ["energiaa.opus","killall.opus","seis.opus"]
  let vlc = vlcCmd vlcH

  -- Unifi motion
  (_, pajaMotion) <- activityDetect 120000000 "inotifywait -qmr --format x /mnt/jako/valvonta/a47395ea-4762-3e3c-9c9f-ede2780bcdaa -e create"

  -- Introduce variables for digital inputs
  [ swKerhoVasen,
    swKerhoOikea,
    swAuki,
    swPois,
    loadVideotykki,
    _,
    _,
    loadKerhoRasia ] <- fst <$> (pollMany $ readInputBits master 2 0 8)
    
  [ swPajaVasen,
    swPajaOikea,
    swMaalaus,
    _,
    _,
    _,
    _,
    loadPaja ] <- fst <$> (pollMany $ readInputBits master 1 0 8)

  -- Viivekatkasiija
  viiveKerhoSahkot <- atomically $ swKerhoOikea >>= newTVar

  delayOffSwitch swKerhoOikea 4000000
    (vlc "goto 4")
    (atomically $ writeTVar viiveKerhoSahkot False)
    (atomically (writeTVar viiveKerhoSahkot True) >> vlc "goto 5")

  -- Remote override
  overrideKerhoSahkot <- newTVarIO False
  overrideKerhoValot  <- newTVarIO False
  overrideDoors       <- newTVarIO False

  let ovetAuki    = (||) <$> swAuki <*> readTVar overrideDoors
      kerhoSahkot = (||) <$> readTVar viiveKerhoSahkot <*> readTVar overrideKerhoSahkot
      kerhoValot  = (||) <$> swKerhoVasen <*> readTVar overrideKerhoValot
      tykkiOhjaus = (&&) <$> kerhoValot <*> (not <$> loadVideotykki)
      pajaValot   = (||) <$> liftWithRetry pajaMotion <*> swPajaOikea
      swPaikalla  = (||) <$> swAuki <*> (not <$> swPois) -- Paikalla tai ovet auki

  -- Connect variables to given relays
  wire kerhoSahkot  (writeBit master 2 0)
  wire kerhoValot   (writeBit master 2 1)
  wire tykkiOhjaus  (writeBit master 2 2) -- Tykin valot

  -- Pajan sähköt
  wire swPajaOikea   (writeBit master 1 0)

  -- Valot pajaan liikkeellä, varastoon kytkimellä
  wire pajaValot    (writeBit master 1 1)
  wire swPajaVasen  (writeBit master 1 2)

  -- Maalaushuoneessa on toggle
  maalausValot <- newTVarIO False
  toggleButton True swMaalaus maalausValot
  wire (readTVar maalausValot) (writeBit master 1 3)

  -- Pajan hätäseis ja kerhon pistorasioiden sulake
  hataSeis <- fst <$> loadSense swPajaOikea loadPaja 500000
  kerhoKuorma <- fst <$> loadSense swKerhoOikea loadKerhoRasia 500000

  pushButton hataSeis nop $ vlc "goto 3"

  -- Door control
  wire ovetAuki (write4chRelay master 3 1) -- Ulko-ovi
  wire ovetAuki (write4chRelay master 3 2) -- Kerhon ovi
  wire ovetAuki (write4chRelay master 3 3) -- Varaston ovi

  -- Door control stream
  doorChan <- newTChanIO
  forkIO $ forever $ do
    -- Receive item, open the doors, and start delay
    delay <- atomically $ do
      writeTVar overrideDoors True
      readTChan doorChan :: STM Int
    delayVar <- registerDelay delay
    -- If no opening commands are received in given timeout, close doors
    atomically $ do
      isEmpty <- isEmptyTChan doorChan
      elapsed <- readTVar delayVar
      case (isEmpty, elapsed) of
        (False, _)   -> pure ()
        (True, True) -> writeTVar overrideDoors False
        _            -> retry

  -- Golffataan vähän monadeilla
  let valotJossakin = or <$> sequence [ swKerhoVasen
                                      , readTVar viiveKerhoSahkot
                                      , swPajaOikea
                                      , readTVar maalausValot
                                      ]

  -- UNIX socket API
  let m = fromList [("kerho-valot", readAction swKerhoVasen)
                   ,("kerho-sahkot", readonly viiveKerhoSahkot)
                   ,("paja-valot", readAction swPajaVasen)
                   ,("paja-sähköt", readAction swPajaOikea)
                   ,("paja-seis", readAction hataSeis)
                   ,("maalaus-valot", readonly maalausValot)
                   ,("kerho-sahkot-ohitus", readwrite overrideKerhoSahkot)
                   ,("kerho-valot-ohitus", readwrite overrideKerhoValot)
                   ,("ovet", action $ writeTChan doorChan)
                   ,("ovet_auki", readAction ovetAuki)
                   ]
  forkIO $ listenJsonQueries m "/tmp/automaatio"

  -- Start some monitors
  q <- newMonitorQueue
  addWatches q [("kerhotila-valot", swKerhoVasen)
               ,("kerhotila-sähköt-katkasin", swKerhoOikea)
               ,("kerhotila-sähköt", readTVar viiveKerhoSahkot)
               ,("työpaja-valot", swPajaVasen)
               ,("työpaja-sähköt", swPajaOikea)
               ,("kerhotila-kuorma", kerhoKuorma)
               ,("työpaja-hätäseis", hataSeis)
               ,("maalaushuone-valot", readTVar maalausValot)
               ,("tila-poissa", swPois)
               ,("tila-paikalla", swPaikalla)
               ,("tila-auki", swAuki)
               ,("powered", valotJossakin)
               ,("ovet-auki", ovetAuki)
               ]
  forkIO $ runMonitor stdout q

  putStrLn "Up and running"
  
  atomically $ waitFailure $ getStats master
