module Control.Hackbus.Logging where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

-- |Run monitor with given name, queue and input list of STM variables
runMonitor :: (Traversable t, Eq a, Show a)
           => String
           -> TQueue String
           -> t (String, STM a)
           -> IO (t ThreadId)
runMonitor name queue list = mapM (forkIO . watch (writeTQueue queue . simpleFormat name)) list

-- |Stop running monitor
stopMonitor :: Traversable t => t ThreadId -> IO ()
stopMonitor = mapM_ killThread

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
