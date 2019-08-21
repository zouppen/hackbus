module Control.Hackbus.Logging where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import System.IO

-- |Watch named STM variables for changes
addWatches :: (Traversable t, Eq a, Show a)
           => String
           -> TQueue String
           -> t (String, STM a)
           -> IO (t ThreadId)
addWatches name q = mapM (forkIO . watch (writeTQueue q . simpleFormat name))

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

-- |Formatter for simple things which have sane 
simpleFormat :: Show a => String -> (String, a) -> String
simpleFormat name (k,v) = name ++ " " ++ k ++ ": " ++ show v

-- |Just a mnemonic for creating a new queue
newMonitorQueue :: IO (TQueue String)
newMonitorQueue = newTQueueIO

-- |Run monitor which prints to given handle
runMonitor h q = forever $ atomically (readTQueue q) >>= hPutStrLn h
