{-# LANGUAGE RecordWildCards, TupleSections #-}
module System.Hardware.Modbus
  ( B.Parity(..)
  , B.ModbusException
  , B.newRTU
  , B.connect
  , Master
  , getStats
  , runMaster
  , readInputBits
  , writeBit
  , writeRegister
  , writeBitRegister
  , waitFailure
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (catch, throw)
import Control.Monad (forever, when)
import Data.Word
import System.Hardware.Modbus.Types
import qualified System.Hardware.Modbus.LowLevel as B

data Master = Master { opQueue  :: TQueue Operation
                     , handle   :: B.ModbusHandle
                     , thread   :: ThreadId
                     , stats    :: TVar Stats
                     }

data Operation = Operation { operation :: IO ()
                           , exception :: B.ModbusException -> IO ()
                           }

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

-- |Generic Modbus interaction
modbusInteract :: Master -> Int -> (B.ModbusHandle -> IO a) -> (a -> STM ()) -> STM ()
modbusInteract Master{..} slave command action = do
  writeTQueue opQueue Operation
    { operation = do
        B.setSlave handle slave
        out <- command handle
        atomically $ action out
    , exception = atomically . action . throw
    }

-- |Modbus control command. Does not require return results.
modbusControl :: Master -> Int -> Control (B.ModbusHandle -> IO ())
modbusControl master slave command = do
  var <- newTVar False
  modbusInteract master slave command $ const $ writeTVar var True
  return $ readTVar var >>= check

-- |Read given number of bits from given address. Modbus function code
-- 0x02 (read input status).
readInputBits :: Master -> Int -> Int -> Int -> ([Bool] -> STM ()) -> STM ()
readInputBits master slave addr nb act = modbusInteract master slave (\h -> B.readInputBits h addr nb) act

-- |Relay which is controlled via Modbus function code 0x05 (force
-- single coil)
writeBit :: Master -> Int -> Int -> Control Bool
writeBit master slave addr status = modbusControl master slave $ \h -> B.writeBit h addr status

-- |Write register using Modbus function code 0x06 (preset single
-- register).
writeRegister :: Master -> Int -> Int -> Control Word16
writeRegister master slave addr value = modbusControl master slave $ \h -> B.writeRegister h addr value

-- |Relay which is controlled using 0x0100 for closed and 0x0200 open
-- state via Modbus function code 0x06 (preset single
-- register). Chinese stuff.
writeBitRegister :: Master -> Int -> Int -> Control Bool
writeBitRegister m s a v = writeRegister m s a (if v then 0x0100 else 0x0200)

-- |Read statistics
getStats :: Master -> STM Stats
getStats Master{..} = readTVar stats

-- |Return when failure occurs
waitFailure :: STM Stats -> STM ()
waitFailure stats = do
  Stats{..} <- stats
  when (fails == 0) retry
