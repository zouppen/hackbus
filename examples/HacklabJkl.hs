{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Hackbus.AlarmSystem
import Control.Hackbus.Logging
import Control.Hackbus.Modules.HfEasy
import Control.Hackbus.Persistence
import Control.Hackbus.PeekPoke
import Control.Hackbus.UnixJsonInterface
import Control.Hackbus.UnixSocket (connectUnixSocket, activityDetect)
import Control.Monad
import Data.HashMap.Strict (fromList)
import Data.Scientific (Scientific)
import Media.Streaming.Vlc
import System.Directory
import System.Environment (getArgs)
import System.FilePath (joinPath)
import System.Hardware.Modbus
import System.Hardware.Modbus.Abstractions
import System.Hardware.Modbus.Types (Control)
import System.IO
import System.Posix.Time (epochTime)
import System.Posix.Types (EpochTime)
import System.Process (callCommand)

runListenUnixSocketActivity :: Int -> String -> IO (ThreadId, TReadable Bool)
runListenUnixSocketActivity triggerDelay path = do
  var <- newTVarIO Nothing
  let handler = activityDetect triggerDelay (atomically . writeTVar var . Just)
  tid <- forkIO $ connectUnixSocket handler path
  return (tid, mkTReadable var)

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

-- |Track for the time when arming was lifted the last time. Used for
-- tracking the visitors and nothing too serious.
runUnarmTimeRecorder :: TVar (Maybe EpochTime) -> TVar Bool -> STM ArmedState -> (Char -> IO ()) -> IO ()
runUnarmTimeRecorder timeVar armedVar source beep = watchWithIO source $ \_ new -> do
  armed <- readTVar armedVar
  io <- case (armed, new) of
    (True, Unarmed) -> do
      writeTVar armedVar False
      noIO
    (False, Armed) -> do
      writeTVar armedVar True
      noIO
    (_, Uncertain) -> pure $ do
      -- Recording the time of opening in advance to get the exact
      -- time when doors were opened.
      now <- epochTime
      atomically $ writeTVar timeVar $ Just now
    _ -> noIO
  pure $ do
    io
    beep $ case new of
      Unarmed   -> 't'
      Armed     -> 'h'
      Arming    -> 's'
      Uncertain -> 's'

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

  -- Place where to put stuff
  runtimeDir <- getXdgDirectory XdgData "hackbus"
  createDirectoryIfMissing True runtimeDir
  let persFile = joinPath [runtimeDir, "hacklabjkl.json"]
  
  withPersistence 60 persFile $ logic master

logic :: Master -> Persistence -> IO ()
logic master pers = do
  -- Audio source
  vlcH <- vlcStart (Just "/home/joell") ["energiaa.opus","killall.opus","seis.opus","saunominen.flac"]
  let vlc = vlcCmd vlcH
  vlc "volume 220"

  -- Prepare data acquisition board
  daqH <- openFile "/dev/piipperi" ReadWriteMode
  hSetBuffering daqH NoBuffering
  
  -- Beeper
  let beep c = hPutStr daqH [c]

  -- Energy meter
  energyVar <- atomically $ newTVarPers pers "energy" (0 :: Integer)
  forkIO $ forever $ do
    -- One tick means 1 watt hour of energy
    hGetChar daqH
    atomically $ modifyTVar' energyVar (+1)

  -- Unifi motion
  (_,pajaMotion) <- runListenUnixSocketActivity 120000000 "/run/kvm/unifi/liiketunnistin"

  -- Netwjork SP5 in kitchen
  netwjork <- runHfEasyRelay "http://10.0.6.32/state"

  -- Introduce variables for digital inputs
  [ swKerhoVasen,
    swKerhoOikea,
    swAuki,
    swPois,
    loadVideotykki,
    swUutiset,
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

  -- Initial state of alarm is the state of "home" switch
  lockFlagVar <- newTVarIO False
  armingState <- atomically $ newTVarPers pers "armed" Unarmed
  armedVar <- atomically $ newTVarPers pers "armedTmp" False
  unarmedAt <- atomically $ newTVarPers pers "unarmedAt" Nothing
  forkIO $ runUnarmTimeRecorder unarmedAt armedVar (readTVar armingState) beep

  -- Remote override
  overrideKerhoSahkot <- newTVarIO False
  overrideKerhoValot  <- newTVarIO False
  overrideDoors       <- newTVarIO False
  saunaReadyState     <- atomically $ newTVarPers pers "sauna" False
  saunaHeatsState     <- atomically $ newTVarPers pers "sauna_heats" False

  let ovetAuki    = (||) <$> swAuki <*> readTVar overrideDoors
      isUnarmed   = (== Unarmed) <$> readTVar armingState
      kerhoSahkot = (||) <$> isUnarmed <*> readTVar overrideKerhoSahkot
      kerhoValot  = (||) <$> swKerhoVasen <*> readTVar overrideKerhoValot
      tykkiOhjaus = (&&) <$> kerhoValot <*> (not <$> loadVideotykki)
      pajaValot   = (||) <$> peekWithRetry pajaMotion <*> swPajaOikea
      swPaikalla  = (||) <$> swAuki <*> (not <$> swPois) -- Paikalla tai ovet auki

  -- Alarm initial state thingies continue
  forkIO $ runAlarmSystem $ AlarmSystem 60 swPaikalla lockFlagVar armingState

  -- Other info
  inCharge <- atomically $ newTVarPers pers "inCharge" (Nothing :: Maybe String)

  -- Kerhotilan ohjaukset
  wire kerhoSahkot  (writeBit master 2 0)
  wire kerhoValot   (writeBit master 2 1)
  wire tykkiOhjaus  (writeBit master 2 2) -- Tykin valot

  -- Pajan sähköt
  wire swPajaOikea   (writeBit master 1 0)

  -- Valot pajaan liikkeellä, varastoon kytkimellä
  wire pajaValot    (writeBit master 1 1)
  wire swPajaVasen  (writeBit master 1 2)

  -- Maalaushuoneessa on toggle
  maalausValot <- atomically $ newTVarPers pers "maalausValot" False
  toggleButton True swMaalaus maalausValot
  wire (readTVar maalausValot) (writeBit master 1 3)

  -- Stop radiouutiset with a micro switch
  pushButton swUutiset nop $ callCommand "sudo systemctl stop radiouutiset"

  -- Sauna valmis -äänitehoste
  pushButton (readTVar saunaReadyState) nop $ vlc "goto 3"

  -- Pajan hätäseis ja kerhon pistorasioiden sulake
  hataSeis <- fst <$> loadSense swPajaOikea loadPaja 500000
  kerhoKuorma <- fst <$> loadSense swKerhoOikea loadKerhoRasia 500000

  pushButton hataSeis nop $ vlc "goto 4"

  -- Door control
  wire ovetAuki (writeBitRegister master 3 1) -- Ulko-ovi
  wire ovetAuki (writeBitRegister master 3 2) -- Kerhon ovi
  wire ovetAuki (writeBitRegister master 3 3) -- Varaston ovi

  -- Beeper for leave and arrival
  let beeper = do
        state <- readTVar armingState
        pure (state == Uncertain || state == Arming)

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
                                      , isUnarmed
                                      , swPajaOikea
                                      , readTVar maalausValot
                                      ]

  -- UNIX socket API
  let m = fromList [("kerho-valot", readAction swKerhoVasen)
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
                   ,("energy", readonly energyVar)
                   ,("sauna_temp", action $ \t -> do
                        case [(t::Scientific)<40, t>60] of
                          [True,_] -> writeTVar saunaReadyState False
                          [_,True] -> writeTVar saunaReadyState True
                          _        -> pure ()
                        case [t<30, t>34] of
                          [True,_] -> writeTVar saunaHeatsState False
                          [_,True] -> writeTVar saunaHeatsState True
                          _        -> pure ()
                    )
                   ]
  forkIO $ listenJsonQueries m "/tmp/automaatio"

  -- Limited "public" UNIX socket API
  let m = fromList [("open", readAction swAuki)
                   ,("in_charge", readonly inCharge)
                   ,("arming_state", readonly armingState)
                   ,("energy", readonly energyVar)
                   ,("sauna", readonly saunaReadyState)
                   ]
  forkIO $ listenJsonQueries m "/tmp/hackbus_public"

  -- Start some monitors
  q <- newMonitorQueue
  addWatches q [kv "kerhotila-valot" swKerhoVasen
               ,kv "kerhotila-sähköt-katkasin" swKerhoOikea
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
               ,kv "keittio" $ peek $ state netwjork
               ,kv "arming_state" $ peek armingState
               ,kvv "visitor_info" armedVar [read' inCharge, read' unarmedAt]
               ,kv "sauna_heats" $ readTVar saunaHeatsState -- Used by notifier in visitors
               ]
  forkIO $ runMonitor stdout q

  putStrLn "Up and running"
  
  atomically $ waitFailure $ getStats master
