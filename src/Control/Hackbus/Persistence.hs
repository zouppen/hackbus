{-# LANGUAGE RecordWildCards #-}
module Control.Hackbus.Persistence where

import qualified Data.Map.Lazy as M
import Data.Text (Text, unpack)
import Data.Aeson
import Control.Concurrent
import Control.Concurrent.STM
import System.Posix.Signals
import Control.Monad

data PersItem = File Value | Live (STM Encoding)

data Persistence = Persistence
  { store :: TVar (M.Map Text PersItem) -- ^Contains all persistent values
  , file  :: FilePath                   -- ^File to store data to
  , tId   :: ThreadId                   -- ^Thread ID
  }

-- |Load persistence from file.
runPersistence :: FilePath -> IO Persistence
runPersistence file = do
  -- Read JSON or die
  contents <- either error id <$> eitherDecodeFileStrict' file
  -- Create variable and wrap values into dummy STM actions at first
  store <- newTVarIO $ File <$> contents
  -- Register signal handler
  quit <- newTVarIO False
  mainThread <- myThreadId
  installHandler sigTERM (Catch $ atomically $ writeTVar quit True) Nothing
  -- Main loop doing all the magic
  tId <- forkIO $ persLoop file mainThread (readTVar quit) (readTVar store)
  return Persistence{..}

-- |Create new TVar which is backed in persistent storage.
newTVarPers :: (FromJSON a, ToJSON a)
            => Persistence  -- ^Persistence object
            -> Text         -- ^Key name
            -> a            -- ^Default value if key doesn't exist
            -> STM (TVar a) -- ^New transaction variable
newTVarPers Persistence{..} name def = do
  store' <- readTVar store
  -- Finding stored value. Using supplied default if given key doesn't
  -- exist.
  initial <- case M.lookup name store' of
    Nothing       -> pure def
    Just (File a) -> case fromJSON a of
      Error msg -> error $ "Persistent value parse error on " ++ unpack name ++ ": " ++ msg
      Success b -> pure b
    Just (Live _) -> error $ "Persistent value already registered: " ++ unpack name
  var <- newTVar initial
  writeTVar store $ M.insert name (Live (toEncoding <$> readTVar var)) store'
  return var

persLoop :: FilePath -> ThreadId -> STM Bool -> STM (M.Map Text PersItem) -> IO ()
persLoop file mainThread quit store = forever $ do
  time <- registerDelay 10000000 -- 10 sec
  die <- atomically $ do
    quit' <- quit
    time' <- readTVar time
    case (quit', time') of
      (True, _) -> pure True
      (_, True) -> pure False
      _         -> retry
  -- Now it's time to write
  -- TODO
  -- Stop if it was a signal
  when die $ killThread mainThread
