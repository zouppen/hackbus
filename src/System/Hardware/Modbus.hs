{-# LANGUAGE RecordWildCards, TupleSections #-}
module System.Hardware.Modbus
  ( B.Parity(..)
  , B.ModbusException
  , B.newRTU
  , B.connect
  , Master
  , Stats
  , Call
  , getStats
  , runMaster
  , sync
  , readInputBits
  , writeBit
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (catch, throw)
import Control.Monad (forever)
import qualified System.Hardware.Modbus.LowLevel as B

data Master = Master { opQueue  :: TQueue Operation
                     , handle   :: B.ModbusHandle
                     , thread   :: ThreadId
                     , stats    :: TVar Stats
                     }

data Stats = Stats { writes   :: Int
                   , retries  :: Int
                   , fails    :: Int
                   } deriving (Show, Eq, Ord)

data Operation = Operation { operation :: IO ()
                           , exception :: B.ModbusException -> IO ()
                           }

type Callback a = a -> STM ()
type Call a = Callback a -> STM ()

-- |Internally handle a single Modbus operation
handleModbus :: TVar Stats -> Operation -> IO ()
handleModbus statsVar Operation{..} = catch
  (retryAction retryMax operation >>= statsUpdateOk)
  (\e -> exception e >> statsUpdateFail)
  where
    statsUpdateOk (_,left) = atomically $ do
      Stats{..} <- readTVar statsVar
      writeTVar statsVar $ Stats{ writes  = succ writes
                                , retries = retries + retryMax - left
                                , fails   = fails
                                }
    statsUpdateFail = atomically $ do
      Stats{..} <- readTVar statsVar
      writeTVar statsVar $ Stats{ writes  = succ writes
                                , retries = retries + retryMax
                                , fails   = fails + 1
                                }
    retryMax = 5

-- |Retry given number of times and return the result and the number
-- of retries left.
retryAction :: Int -> IO a -> IO (a, Int)
retryAction 0 action = (,0) <$> action 
retryAction n action = c realAction $ \e -> retryAction (n-1) action
  where
    c :: IO a -> (B.ModbusException -> IO a) -> IO a
    c = catch
    realAction = (,n) <$> action

-- Public parts

runMaster :: B.ModbusHandle -> IO Master
runMaster handle = do
  opQueue <- newTQueueIO
  stats <- newTVarIO $ Stats 0 0 0
  thread <- forkIO $ forever $ do
    task <- atomically $ readTQueue opQueue
    handleModbus stats task
    threadDelay 5000
  return Master{..}

readInputBits :: Master -> Int -> Int -> Int -> Call [Bool]
readInputBits Master{..} slave addr nb target = do
  writeTQueue opQueue Operation
    { operation = do
        B.setSlave handle slave
        out <- B.readInputBits handle addr nb
        atomically $ target out
    , exception = atomically . target . throw
    }
  return ()
  -- TODO add target function retry check. Should never block.

-- |Relay which is controlled via Modbus function code 0x05 (force
-- single coil)
writeBit :: Master -> Int -> Int -> Bool -> STM (STM ())
writeBit Master{..} slave addr status = do
  var <- newTVar False
  writeTQueue opQueue Operation
    { operation = do
        B.setSlave handle slave
        B.writeBit handle addr status
        atomically $ writeTVar var True
    , exception = atomically . writeTVar var . throw
    }
  return $ readTVar var >>= check

-- |Run action synchronously.
sync :: STM (STM a) -> IO a
sync act = atomically act >>= atomically

-- |Read statistics
getStats :: Master -> STM Stats
getStats Master{..} = readTVar stats
