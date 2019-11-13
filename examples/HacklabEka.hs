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

  --let swKerhoOikea = return True
  
  -- Introduce variables for digital inputs
  [ swKerhoVasen,
    swKerhoOikea,
    _,
    _,
    _,
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
  viivePajaSahkot <- atomically $ swPajaOikea >>= newTVar

  delayOffSwitch swKerhoOikea 4000000
    (vlc "goto 4")
    (atomically $ writeTVar viiveKerhoSahkot False)
    (atomically (writeTVar viiveKerhoSahkot True) >> vlc "goto 5")

  delayOffSwitch swPajaOikea 4000000
    nop
    (atomically $ writeTVar viivePajaSahkot False)
    (atomically $ writeTVar viivePajaSahkot True)

  -- Remote override
  overrideKerhoSahkot <- newTVarIO False
  overrideKerhoValot  <- newTVarIO False

  let kerhoSahkot = or <$> sequence [ readTVar viiveKerhoSahkot
                                    , readTVar overrideKerhoSahkot
                                    ]

  let kerhoValot = or <$> sequence [ swKerhoVasen
                                   , readTVar overrideKerhoValot
                                   ]

  -- Connect variables to given relays
  wire kerhoSahkot  (writeBit master 2 0)
  wire kerhoValot   (writeBit master 2 1)

  -- Pajan sähköt
  wire (readTVar viivePajaSahkot)   (writeBit master 1 0)

  -- Valot myös varastoon samalla kun pajaan
  wire swPajaVasen   (writeBit master 1 1)
  wire swPajaVasen   (writeBit master 1 2)

  -- Maalaushuoneessa on toggle
  maalausValot <- newTVarIO False
  toggleButton True swMaalaus maalausValot
  wire (readTVar maalausValot) (writeBit master 1 3)

  -- Pajan hätäseis ja kerhon pistorasioiden sulake
  hataSeis <- fst <$> loadSense (readTVar viivePajaSahkot) loadPaja 500000
  kerhoKuorma <- fst <$> loadSense swKerhoOikea loadKerhoRasia 500000

  pushButton hataSeis nop $ vlc "goto 3"

  -- Golffataan vähän monadeilla
  let valotJossakin = or <$> sequence [ swKerhoVasen
                                      , readTVar viiveKerhoSahkot
                                      , swPajaVasen
                                      , swPajaOikea
                                      , readTVar maalausValot
                                      ]
  
  pushButton valotJossakin
    (callCommand "~/matrix-hacklab 'Hacklabin sähköt sammuivat.';sudo systemctl stop aikamerkki.timer")
    (callCommand "~/matrix-hacklab 'Hacklabilla on nyt sähköt päällä!';sudo systemctl start aikamerkki.timer")

  -- UNIX socket API
  let m = fromList [("kerho-valot", readAction swKerhoVasen)
                   ,("kerho-sahkot", readonly viiveKerhoSahkot)
                   ,("paja-valot", readAction swPajaVasen)
                   ,("paja-sähköt", readonly viivePajaSahkot)
                   ,("paja-seis", readAction hataSeis)
                   ,("maalaus-valot", readonly maalausValot)
                   ,("kerho-sahkot-ohitus", readwrite overrideKerhoSahkot)
                   ,("kerho-valot-ohitus", readwrite overrideKerhoValot)
                   ]
  listenJsonQueries m "/tmp/automaatio"

  putStrLn "Up and running"

  -- Start some monitors
  q <- newMonitorQueue
  addWatches "log" q [("kerhotila-valot", swKerhoVasen)
                     ,("kerhotila-sähköt-katkasin", swKerhoOikea)
                     ,("kerhotila-sähköt", readTVar viiveKerhoSahkot)
                     ,("työpaja-valot", swPajaVasen)
                     ,("työpaja-sähköt-katkaisin", swPajaOikea)
                     ,("työpaja-sähköt", readTVar viivePajaSahkot)
                     ,("kerhotila-kuorma", kerhoKuorma)
                     ,("työpaja-hätäseis", hataSeis)
                     ,("maalaushuone-valot", readTVar maalausValot)
                     ]
  runMonitor stdout q
  
  atomically $ waitFailure $ getStats master
