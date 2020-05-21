{-# LANGUAGE RecordWildCards #-}
module Control.Hackbus.Persistence where

import qualified Data.Map.Lazy as M
import Data.Text (Text, unpack)
import Data.Aeson
import Control.Concurrent.STM

data PersItem = File Value | Live (STM Value)

data Persistence = Persistence
  { store :: TVar (M.Map Text PersItem) -- ^Contains all persistent values
  , file  :: FilePath                   -- ^File to store data to
  }

-- |Load persistence from file.
newPersistence :: FilePath -> IO Persistence
newPersistence file = do
  -- Read JSON or die
  contents <- either error id <$> eitherDecodeFileStrict' file
  -- Create variable and wrap values into dummy STM actions at first
  store <- newTVarIO $ File <$> contents
  return Persistence{..}

-- |Create new TVar which is backed in persistent storage.
newTPersVar :: (FromJSON a, ToJSON a)
            => Persistence  -- ^Persistence object
            -> Text         -- ^Key name
            -> a            -- ^Default value if key doesn't exist
            -> STM (TVar a) -- ^New transaction variable
newTPersVar Persistence{..} name def = do
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
