module Control.Hackbus.Logging where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import System.IO
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson (ToJSON, toJSON, encode)
import Data.Text (Text)

import Control.Hackbus.JsonCommands

-- |Watch named STM variables for changes
addWatches :: Traversable t => a -> t (a -> IO ()) -> IO (t ThreadId)
addWatches q = mapM $ \f -> forkIO $ f q

-- |Stop running watches
stopWatches :: Traversable t => t ThreadId -> IO ()
stopWatches = mapM_ killThread

-- |Watch changes in a given STM variable. When value changes, run
-- given action.
watch :: Eq v => STM v -> STM () -> IO ()
watch source notify = do
  oldVar <- atomically $ source >>= newTVar 
  forever $ atomically $ do
    new <- source
    old <- readTVar oldVar
    when (old == new) retry
    writeTVar oldVar new
    notify

-- |JSON report formatter
jsonReport :: ToJSON a => Text -> a -> B.ByteString
jsonReport k v = encode $ Report k $ toJSON v

-- |Add element to queue with a key and a single value which is
-- printed only when it changes.
kv :: (Eq a, ToJSON a) => Text -> STM a -> TQueue B.ByteString -> IO ()
kv k v q = watch v $ do
  v' <- v
  writeTQueue q $ jsonReport k v'

-- |Add element to queue with a key and a value v1 which is printed
-- only if it changes. While printing, it is printed with value of v2.
kvv :: (Eq a, ToJSON a, ToJSON b) => Text -> STM a -> STM b -> TQueue B.ByteString -> IO ()
kvv k v1 v2 q = watch v1 $ do
  v1' <- v1
  v2' <- v2
  writeTQueue q $ jsonReport k (v1',v2')

-- |Just a mnemonic for creating a new queue
newMonitorQueue :: IO (TQueue B.ByteString)
newMonitorQueue = newTQueueIO

-- |Run monitor which prints to given handle
runMonitor :: Handle -> TQueue B.ByteString -> IO b
runMonitor h q = forever $ atomically (readTQueue q) >>= act
  where act msg = B.hPut h $ B.snoc msg '\n'
