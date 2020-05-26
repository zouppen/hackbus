{-# LANGUAGE RecordWildCards #-}
module Control.Hackbus.Persistence ( Persistence
                                   , withPersistence
                                   , newTVarPers
                                   ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Text (Text, unpack)
import Data.Aeson
import Control.Concurrent
import Control.Exception
import Control.Concurrent.STM
import Control.Monad
import System.Directory (doesFileExist)
import Control.Monad.Loops (iterateUntil)

data PersState = Running | Stopping | Stopped  deriving (Show, Eq)

data PersItem = File Value | Live (STM Value)

data Persistence = Persistence
  { store :: TVar (HashMap Text PersItem) -- ^Contains all persistent values
  }

-- |Load persistence from file and store it back periodically and in
-- state of an Exception.
withPersistence
  :: Int                    -- ^How often to save state in normal operation in seconds.
  -> FilePath               -- ^File to store the state to.
  -> (Persistence -> IO ()) -- ^IO action to perform with persistent vars
  -> IO ()
withPersistence interval file act = do
  -- Read JSON or die
  exists <- doesFileExist file
  contents <- if exists
    then either error id <$> eitherDecodeFileStrict' file
    else pure M.empty
  -- Create variable and wrap values into dummy STM actions at first
  store <- newTVarIO $ File <$> contents
  -- Register signal handler
  state <- newTVarIO Running
  catch (act Persistence{..}) $ \e -> do
    pure (e::AsyncException) -- Just to nail the type
    -- Ask the thread to stop.
    putStrLn "Saving state..."
    atomically $ writeTVar state Stopping
    atomically $ readTVar state >>= \s -> unless (s==Stopped) retry
    putStrLn "State saved"
  -- Main loop doing all the magic
  void $ forkIO $ persLoop interval file state (readTVar store)

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
  writeTVar store $ M.insert name (Live (toJSON <$> readTVar var)) store'
  return var

persLoop :: Int -> FilePath -> TVar PersState -> STM (HashMap Text PersItem) -> IO ()
persLoop interval file stateVar store = loop $ do
  timeVar <- registerDelay $ 1000000 * interval
  ret <- atomically $ do
    state <- readTVar stateVar
    timeout <- readTVar timeVar
    when (state == Running && not timeout) retry
    pure state
  -- Now it's time to write
  json <- atomically $ store >>= traverse itemToValue
  encodeFile file json
  pure ret
  where
    loop a = finally (void $ iterateUntil (==Stopping) a) sayQuit
    sayQuit = atomically $ writeTVar stateVar Stopped

itemToValue :: PersItem -> STM Value
itemToValue item = case item of
  File a -> pure a
  Live a -> a
