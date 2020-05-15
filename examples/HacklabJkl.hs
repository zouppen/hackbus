{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Hackbus.Logging
import Control.Hackbus.UnixJsonInterface
import Control.Hackbus.UnixSocket (connectUnixSocket, activityDetect)
import Control.Hackbus.AlarmSystem
import Control.Monad
import Data.Map.Lazy (fromList)
import Data.Time.Clock.POSIX
import System.Environment (getArgs)
import System.Hardware.Modbus
import System.Hardware.Modbus.Abstractions
import System.IO
import System.Process
import Media.Streaming.Vlc
import System.Hardware.Modbus.Types (Control)

runListenUnixSocketActivity :: Int -> String -> IO (ThreadId, TReadable Bool)
runListenUnixSocketActivity triggerDelay path = do
  var <- newTVarIO Nothing
  let handler = activityDetect triggerDelay (atomically . writeTVar var . Just)
  tid <- forkIO $ connectUnixSocket handler path
  return (tid, TReadable var)

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

-- |Track for the time when a certain state change has taken place
forkStateStartTimeRecorder :: Eq a => STM a -> a -> IO (ThreadId, TReadable POSIXTime)
forkStateStartTimeRecorder source state = do
  timeVar <- newTVarIO Nothing
  tid <- forkIO $ watchWith source comparator $ pure $ do
    time <- getPOSIXTime
    atomically $ writeTVar timeVar $ Just time
  return (tid, TReadable timeVar)
  where comparator old new = old /= new && new == state

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
  (_,pajaMotion) <- runListenUnixSocketActivity 120000000 "/run/kvm/unifi/liiketunnistin"

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
      pajaValot   = (||) <$> peekWithRetry pajaMotion <*> swPajaOikea
      swPaikalla  = (||) <$> swAuki <*> (not <$> swPois) -- Paikalla tai ovet auki

  -- Other info
  inCharge <- newTVarIO (Nothing :: Maybe String)
  
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
  wire ovetAuki (writeBitRegister master 3 1) -- Ulko-ovi
  wire ovetAuki (writeBitRegister master 3 2) -- Kerhon ovi
  wire ovetAuki (writeBitRegister master 3 3) -- Varaston ovi

  -- Initial state of alarm is the state of "home" switch
  lockFlagVar <- newTVarIO False
  armingState <- do
    paikalla <- atomically swPaikalla
    -- This simple until we can store states
    newTVarIO $ if paikalla then Unarmed else Armed
  forkIO $ runAlarmSystem $ AlarmSystem 30000000 swPaikalla lockFlagVar armingState
  (_, lastUnarmed) <- forkStateStartTimeRecorder (readTVar armingState) Unarmed

  -- Door control stream
  doorChan <- newTChanIO
  forkIO $ forever $ do
    -- Receive item, open the doors, and start delay
    delay <- atomically $ do
      writeTVar overrideDoors True
      writeTVar lockFlagVar True -- Signal the alarm system as well
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
                   ,("in_charge", readwrite inCharge)
                   ,("ovet_auki", readAction ovetAuki)
                   ,("arming_state", readonly armingState)
                   ]
  forkIO $ listenJsonQueries m "/tmp/automaatio"

  -- Start some monitors
  q <- newMonitorQueue
  addWatches q [kv "kerhotila-valot" swKerhoVasen
               ,kv "kerhotila-sähköt-katkasin" swKerhoOikea
               ,kv "kerhotila-sähköt" $ readTVar viiveKerhoSahkot
               ,kv "työpaja-valot" swPajaVasen
               ,kv "työpaja-sähköt" swPajaOikea
               ,kv "kerhotila-kuorma" kerhoKuorma
               ,kv "työpaja-hätäseis" hataSeis
               ,kv "maalaushuone-valot" $ readTVar maalausValot
               ,kv "tila-poissa" swPois
               ,kv "tila-paikalla" swPaikalla
               ,kv "tila-auki" swAuki
               ,kv "powered" valotJossakin
               ,kv "ovet-auki" ovetAuki
               ,kv "in_charge" $ readTVar inCharge
               ,kvv "arming_state" armingState [read' inCharge, read' lastUnarmed]
               ]
  forkIO $ runMonitor stdout q

  putStrLn "Up and running"
  
  atomically $ waitFailure $ getStats master
