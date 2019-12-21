module Control.Hackbus.Logging where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import System.IO
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson (ToJSON, toJSON, encode)
import Data.Map.Lazy (singleton)
import Data.Text (Text)

import Control.Hackbus.JsonCommands

-- |Watch named STM variables for changes
addWatches :: (Traversable t, Eq a, ToJSON a)
           => TQueue B.ByteString
           -> t (Text, STM a)
           -> IO (t ThreadId)
addWatches q = mapM (forkIO . watch (writeTQueue q . jsonFormat))

-- |Stop running watches
stopWatches :: Traversable t => t ThreadId -> IO ()
stopWatches = mapM_ killThread

-- |Watch changes in a given STM variable
watch :: Eq v => ((k, v) -> STM ()) -> (k, STM v) -> IO ()
watch enq (key,source) = do
  oldVar <- atomically $ source >>= newTVar 
  forever $ atomically $ do
    new <- source
    old <- readTVar oldVar
    when (old == new) retry
    writeTVar oldVar new
    enq (key,new)

-- |JSON formatter
jsonFormat :: ToJSON a => (Text, a) -> B.ByteString
jsonFormat (k,v) = encode $ Return $ singleton k $ toJSON v

-- |Just a mnemonic for creating a new queue
newMonitorQueue :: IO (TQueue B.ByteString)
newMonitorQueue = newTQueueIO

-- |Run monitor which prints to given handle
runMonitor :: Handle -> TQueue B.ByteString -> IO b
runMonitor h q = forever $ atomically (readTQueue q) >>= act
  where act msg = B.hPut h $ B.snoc msg '\n'
