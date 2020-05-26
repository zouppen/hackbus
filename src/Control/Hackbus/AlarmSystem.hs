{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
module Control.Hackbus.AlarmSystem (ArmedState(..), AlarmSystem(..), runAlarmSystem) where

import Control.Concurrent.STM
import Data.Aeson
import GHC.Generics

data ArmedState = Unarmed   -- ^No alarms should trigger
                | Arming    -- ^No alarms should trigger (yet)
                | Armed     -- ^Alarms should trigger
                | Uncertain -- ^Alarms should be held for triggering if the next state is Armed
                deriving (Eq, Show, Generic)

data AlarmSystem = AlarmSystem
  { delay    :: Int             -- ^Delay in seconds for arrival and leave
  , atHome   :: STM Bool        -- ^State of "at home" switch
  , lockFlag :: TVar Bool       -- ^Flag for unlock event
  , armState :: TVar ArmedState -- ^Resulting state if alarm is on currently
  }

instance ToJSON ArmedState where
    toEncoding = genericToEncoding defaultOptions

-- |When alarm is on.
armed AlarmSystem{..} = atomically $ do
  -- Wait for door unlocking
  readTVar lockFlag >>= check
  writeTVar lockFlag False
  -- Alerts should be held in case they are real (no-one shows up)
  pure Uncertain

-- |Known person opened the door. Wait for delay or home switch state
-- change, whichever comes first.
uncertain AlarmSystem{..} = do
  delayVar <- registerDelay $ 1000000 * delay
  atomically $ do
    isElapsed <- readTVar delayVar
    isAtHome <- atHome
    case (isAtHome, isElapsed) of
      (True, _) -> pure Unarmed
      (_, True) -> pure Armed -- Person disappeared
      _         -> retry

-- |When alarm is off
unarmed AlarmSystem{..} = atomically $ do
  -- Wait until someone turns the away switch. Then clear any lock
  -- activity before waiting new.
  atHome >>= check . not
  writeTVar lockFlag False
  pure Arming

-- |When alarm is going on
arming AlarmSystem{..} = do
  -- Wait delay before arming.
  delayVar <- registerDelay $ 1000000 * delay
  atomically $ do
    isElapsed    <- readTVar delayVar
    suddenUnlock <- readTVar lockFlag
    case (isElapsed, suddenUnlock) of
      (True, _) -> pure Armed
      (_, True) -> pure Uncertain -- Sudden reopen, another delay
      _         -> retry

runAlarmSystem :: AlarmSystem -> IO ()
runAlarmSystem system@AlarmSystem{..} = atomically (readTVar armState) >>= loop
  where
    loop state = do
      new <- case state of
        Unarmed   -> unarmed system
        Armed     -> armed system
        Uncertain -> uncertain system
        Arming    -> arming system
      atomically $ writeTVar armState new
      loop new
