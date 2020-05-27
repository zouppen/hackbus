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

newtype Persistence = Persistence (TVar (HashMap Text PersItem))

data PersItem = File Value | Live (STM Value) | Subtree Persistence

-- |Wrapping the result as an unparsed value.
instance FromJSON PersItem where
  parseJSON = pure . File

-- |Load persistence from file and store it back periodically and in
-- case of an Exception. Exceptions are rethrown so it's possible to
-- catch exceptions generated by the action.
withPersistence
  :: Int                   -- ^How often to save state in seconds.
  -> FilePath              -- ^File to store the state to.
  -> (Persistence -> IO a) -- ^IO action to perform with persistent vars
  -> IO a
withPersistence interval file act = do
  -- Read JSON or die
  exists <- doesFileExist file
  contents <- if exists
    then either error id <$> eitherDecodeFileStrict' file
    else pure M.empty
  -- Create variable and wrap values into dummy STM actions at first
  pers <- Persistence <$> newTVarIO contents
  -- Start the background task.
  state <- newTVarIO Running
  forkIO $ persLoop interval file state pers
  -- Run user action until exception is raised.
  finally (act pers) $ do
    -- Ask the thread to stop.
    putStrLn "Saving state..."
    atomically $ writeTVar state Stopping
    atomically $ readTVar state >>= \s -> unless (s==Stopped) retry
    putStrLn "State saved."

-- |Create new TVar which is backed in persistent storage.
newTVarPers :: (FromJSON a, ToJSON a)
            => Persistence  -- ^Persistence object
            -> Text         -- ^Key name
            -> a            -- ^Default value if key doesn't exist
            -> STM (TVar a) -- ^New transaction variable
newTVarPers (Persistence pers) name def = do
  pers' <- readTVar pers
  -- Finding stored value. Using supplied default if given key doesn't
  -- exist.
  initial <- case M.lookup name pers' of
    Nothing       -> pure def
    Just (File a) -> case fromJSON a of
      Error msg -> fail $ "Persistent value parse error on " ++ unpack name ++ ": " ++ msg
      Success b -> pure b
    _ -> fail $ "Persistent value already registered: " ++ unpack name
  var <- newTVar initial
  writeTVar pers $ M.insert name (Live (toJSON <$> readTVar var)) pers'
  return var

-- |Create a new subtree which is a different namespace from the
-- parent object (key names between different subtrees may not be
-- unique but subtree name itself must be unique on same level, of
-- course.
newSubtree :: Persistence     -- ^Persistence object
           -> Text            -- ^Key name
           -> STM Persistence -- ^New subtree
newSubtree (Persistence pers) name = do
  pers' <- readTVar pers
  -- Finding stored value. Using supplied default if given key doesn't
  -- exist.
  initial <- case M.lookup name pers' of
    Nothing       -> pure M.empty
    Just (File a) -> case fromJSON a of
      Error msg -> fail $ "Persistent value parse error on " ++ unpack name ++ ": " ++ msg
      Success b -> pure b
    _ -> fail $ "Persistent value already registered: " ++ unpack name
  subtree <- newTVar initial
  writeTVar pers $ M.insert name (Subtree (Persistence subtree)) pers'
  return $ Persistence subtree

persLoop :: Int -> FilePath -> TVar PersState -> Persistence -> IO ()
persLoop interval file stateVar pers = loop $ do
  timeVar <- registerDelay $ 1000000 * interval
  ret <- atomically $ do
    state <- readTVar stateVar
    timeout <- readTVar timeVar
    when (state == Running && not timeout) retry
    pure state
  -- Now it's time to write
  json <- atomically $ itemToValue $ Subtree pers
  encodeFile file json
  pure ret
  where
    loop a = finally (void $ iterateUntil (==Stopping) a) sayQuit
    sayQuit = atomically $ writeTVar stateVar Stopped

-- |Recursively convert given persistent item to JSON.
itemToValue :: PersItem -> STM Value
itemToValue item = case item of
  File a -> pure a
  Live a -> a
  Subtree (Persistence var) -> do
    itemMap <- readTVar var
    valueMap <- traverse itemToValue itemMap
    pure $ toJSON valueMap
