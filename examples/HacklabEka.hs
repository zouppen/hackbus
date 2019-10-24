module Main where

import Control.Concurrent.STM
import System.Environment (getArgs)
import System.Hardware.Modbus
import System.Hardware.Modbus.Abstractions
import Control.Hackbus.Logging

import System.IO
import System.Process
import Control.Monad
import Control.Concurrent

cmd = "DISPLAY= vlc --play-and-stop --no-playlist-autostart ~/energiaa.opus ~/killall.opus ~/seis.opus"

hCmd :: Handle -> String -> IO ()
hCmd h cmd = do
  hPutStrLn h cmd
  hFlush h

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
  (Just commandH, _, _, procH) <- createProcess (shell cmd){ std_in = CreatePipe }
  let vlc = hCmd commandH

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

  -- Connect variables to given relays
  wire (readTVar viiveKerhoSahkot)  (writeBit master 2 0)
  wire swKerhoVasen  (writeBit master 2 1)

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
    (callCommand "~/matrix-hacklab 'Hacklabin sähköt sammuivat.'")
    (callCommand "~/matrix-hacklab 'Hacklabilla on nyt sähköt päällä!'")

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
