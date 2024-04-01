{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
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
import Control.Hackbus.Math.Sauna
import Control.Monad
import Data.HashMap.Strict (fromList, union)
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
import GHC.Generics
import Data.Aeson

-- |See Sauna module for more info
labSauna :: SaunaConf
labSauna = SaunaConf 30 35 60 18

type DelayVar = TVar (STM Bool)

-- |Create new delay var, with given start state (True: triggered or False: cancelled)
newDelayVar :: Bool -> STM DelayVar
newDelayVar = newTVar . pure

-- |Refresh delay timeout.
delayRefresh :: DelayVar -> Int -> IO ()
delayRefresh var delay = do
  tv <- registerDelay delay
  atomically $ writeTVar var $ readTVar tv

-- |Cancel timer (keep non-triggered state)
delayOff :: DelayVar -> IO ()
delayOff var = atomically $ writeTVar var $ pure False

-- |Test variable state. Returns True if delay triggered, False otherwise.
delayTest :: DelayVar -> STM Bool
delayTest var = join $ readTVar var

-- |Lengthen the "off" state duration. Useful for leave delay for
-- alarm systems. Doesn't delay on->off state transformation.
addOffTail :: Int -> STM Bool -> IO (STM Bool)
addOffTail delay stm = do
  (initState, timer) <- atomically $ do
    a <- stm
    var <- newDelayVar a
    pure (a, var)
  pushButtonInit stm (delayOff timer) (delayRefresh timer delay) initState
  pure $ delayTest timer

-- |Lengthen the "on" state duration. Useful for delayed lights, door
-- buttons, etc. Doesn't delay off->on state transformation.
addOnTail :: Int -> STM Bool -> IO (STM Bool)
addOnTail delay stm = addOffTail delay (not <$> stm) >>= pure . fmap not

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

data VenueInfo = VenueInfo
  { isOpen   :: Bool         -- ^True if doors are open
  , inCharge :: Maybe String -- ^Nickname of the person responsible,
                             -- Nothing if venue is empty.
  } deriving (Eq, Show, Generic)

instance ToJSON VenueInfo where
  toEncoding = genericToEncoding defaultOptions

-- |Convert multi-var state of the venue into one single structure.
toVenueInfo :: ArmedState -> Maybe String -> Bool -> VenueInfo
toVenueInfo armedState inChargeRaw isOpen = case (armedState, isOpen) of
  (Armed, _)     -> VenueInfo False Nothing
  (Uncertain, _) -> VenueInfo False Nothing
  _ -> VenueInfo isOpen inChargeRaw

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

  -- Netwjork SP5 in kitchen
  netwjork <- runHfEasyRelay "http://10.0.6.32/state"

  -- Introduce variables for digital inputs
  [ swKerhoVasen,
    swKerhoOikea,
    swAuki,
    swPois,
    loadVideotykki,
    swVerkko,
    motionKaytavaRaw,
    loadKerhoRasia ] <- fst <$> (pollMany $ readInputBits master 2 0 8)
    
  [ swPajaVasenNc,
    swPajaOikea,
    swMaalaus,
    _,
    _,
    _,
    motionPajaRaw,
    loadPaja ] <- fst <$> (pollMany $ readInputBits master 1 0 8)

  -- Initial state of alarm is the state of "home" switch
  lockFlagVar <- newTVarIO False
  armingState <- atomically $ newTVarPers pers "armed" Unarmed
  armedVar <- atomically $ newTVarPers pers "armedTmp" False
  unarmedAt <- atomically $ newTVarPers pers "unarmedAt" Nothing
  forkIO $ runUnarmTimeRecorder unarmedAt armedVar (readTVar armingState) beep
  saunaState <- atomically $ newTVarPers pers "sauna" SaunaOff
  overridePajaSahkot <- atomically $ newTVarPers pers "paja-sahkot-ohitus" False
  ringing <- newTVarIO False

  -- Negate some switches
  let swPajaVasen = not <$> swPajaVasenNc

  -- Viivekytkennät
  oviPainikeRaw <- addOnTail 30000000 swKerhoOikea -- Maalaushuoneen ovikytkin
  pajaMotion    <- addOnTail 120000000 motionPajaRaw -- Pajan valojen liikekytkin
  
  -- Remote override
  overrideKerhoSahkot   <- newTVarIO False
  overrideKerhoValot    <- newTVarIO False
  overridePajaValot     <- newTVarIO False
  overrideDoors         <- newTVarIO False
  overrideKerhoEtuvalot <- newTVarIO False

  let ovetAukiA   = (||) <$> swAuki <*> readTVar overrideDoors
      isUnarmed   = (== Unarmed) <$> readTVar armingState
      isArmed     = (== Armed) <$> readTVar armingState
      oviPainike  = (&&) <$> isUnarmed <*> oviPainikeRaw
      ovetAuki    = (||) <$> ovetAukiA <*> oviPainike
      kerhoSahkot = (||) <$> isUnarmed <*> readTVar overrideKerhoSahkot
      kerhoValot  = (||) <$> swKerhoVasen <*> readTVar overrideKerhoValot
      tykkiOhjaus = (&&) <$> kerhoValot <*> (not <$> ((||) <$> loadVideotykki <*> readTVar overrideKerhoEtuvalot))
      pajaValot   = (||) <$> ((||) <$> pajaMotion <*> swPajaOikea) <*> readTVar overridePajaValot
      swPaikalla  = (||) <$> swAuki <*> (not <$> swPois) -- Paikalla tai ovet auki
      pajaSahkot  = (||) <$> swPajaOikea <*> readTVar overridePajaSahkot

  -- Motion detectors
  let alarmify state = do
        let alarm = (&&) <$> isArmed <*> state
        addOnTail 8000000 alarm
  
  alarmKaytava <- alarmify (not <$> motionKaytavaRaw)
  alarmPaja    <- alarmify motionPajaRaw

  -- Alarm initial state thingies continue
  forkIO $ runAlarmSystem $ AlarmSystem 60 swPaikalla lockFlagVar armingState

  -- Other info
  inCharge <- atomically $ newTVarPers pers "inCharge" (Nothing :: Maybe String)

  -- Kerhotilan ohjaukset
  wire kerhoSahkot  (writeBit master 2 0)
  wire kerhoValot   (writeBit master 2 1)
  wire tykkiOhjaus  (writeBit master 2 2) -- Tykin valot
  wire (readTVar ringing) (writeBit master 2 3) -- Puhelinhälytys

  -- Pajan sähköt
  wire pajaSahkot   (writeBit master 1 0)

  -- Valot pajaan liikkeellä, varastoon kytkimellä
  wire pajaValot    (writeBit master 1 1)
  wire swPajaVasen  (writeBit master 1 2)

  -- Maalaushuoneessa on toggle
  maalausValot <- atomically $ newTVarPers pers "maalausValot" False
  toggleButton True swMaalaus maalausValot
  wire (readTVar maalausValot) (writeBit master 1 3)

  -- Switch internets with this switch
  pushButton swVerkko (callCommand "sudo internet jyu") (callCommand "sudo internet telia")

  -- Sauna valmis -äänitehoste
  pushButton ((==SaunaReady) <$> readTVar saunaState) nop $ vlc "goto 3"

  -- Pajan hätäseis
  hataSeis <- fst <$> loadSense pajaSahkot loadPaja 500000

  pushButton hataSeis nop $ vlc "goto 4"

  -- Door control
  wire ovetAuki (writeBitRegister master 3 1) -- Ulko-ovi
  wire ovetAuki (writeBitRegister master 3 2) -- Kerhon ovi
  wire oviPainike (writeBitRegister master 3 3) -- Varaston ovi

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

  -- UNIX socket API. Define two sets: open to all (public) and
  -- internal control (private).

  let publicApi = fromList
        [("open", readAction swAuki)
        ,("in_charge", readonly inCharge)
        ,("arming_state", readonly armingState)
        ,("energy", readonly energyVar)
        ,("sauna", readonly saunaState)
        ,("ping", readAction $ pure True)
        ]

  let privateApi = fromList
        [("kerho-valot", readAction swKerhoVasen)
        ,("paja-valot", readAction swPajaVasen)
        ,("paja-sahköt", readAction pajaSahkot)
        ,("paja-seis", readAction hataSeis)
        ,("maalaus-valot", readonly maalausValot)
        ,("kerho-sahkot-ohitus", readwrite overrideKerhoSahkot)
        ,("kerho-valot-ohitus", readwrite overrideKerhoValot)
        ,("jaskamode", readwrite overrideKerhoEtuvalot)
        ,("paja-sahkot-ohitus", readwrite overridePajaSahkot)
        ,("valokuvaus", readwrite overridePajaValot)
        ,("ovet", action $ writeTChan doorChan)
        ,("in_charge", readwrite inCharge)
        ,("ovet_auki", readAction ovetAuki)
        ,("sauna_temp", action $ \(t,h) -> modifyTVar saunaState $ evalSauna labSauna t h)
        ]

  let teleApi = fromList
        [("ring-6906", readwrite ringing)
        ]

  forkIO $ listenJsonQueries publicApi "/tmp/hackbus_public"
  forkIO $ listenJsonQueries (privateApi `union` publicApi) "/tmp/automaatio"
  forkIO $ listenJsonQueries teleApi "/home/joell/teleapi"

  -- Start some monitors
  q <- newMonitorQueue
  addWatches q [kv "kerhotila-valot" swKerhoVasen
               ,kv "ovipainike" oviPainike
               ,kv "työpaja-valot" swPajaVasen
               ,kv "työpaja-sähköt" pajaSahkot
               ,kv "työpaja-hätäseis" hataSeis
               ,kv "maalaushuone-valot" $ readTVar maalausValot
               ,kv "tila-poissa" swPois
               ,kv "tila-paikalla" swPaikalla
               ,kv "tila-auki" swAuki
               ,kv "powered" valotJossakin
               ,kv "ovet-auki" ovetAuki
               ,kv "venue" $ toVenueInfo <$> readTVar armingState <*> readTVar inCharge <*> swAuki
               ,kv "keittio" $ peek $ state netwjork
               ,kv "arming_state" $ peek armingState
               ,kvv "visitor_info" armedVar [read' inCharge, read' unarmedAt]
               ,kv "sauna" $ readTVar saunaState -- Used by notifier in visitors
               ,kv "alarm-kaytava" alarmKaytava -- Kerho motion sensor test
               ,kv "alarm-paja" alarmPaja -- Paja motion sensor test
               ,kv "ringing" $ readTVar ringing
               ,kv "internet" $ swVerkko
               ]
  forkIO $ runMonitor stdout q

  putStrLn "Up and running"
  
  atomically $ waitFailure $ getStats master
