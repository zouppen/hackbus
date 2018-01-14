{-# LANGUAGE RecordWildCards, TupleSections #-}
module System.Hardware.Modbus
  ( B.Parity(..)
  , B.ModbusException
  , B.newRTU
  , B.connect
  , Master
  , Stats
  , getStats
  , runMaster
  , sync
  , readInputBits
  , writeBit
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (catch, throw)
import Control.Monad (forever, when)
import qualified Data.Set as S
import qualified System.Hardware.Modbus.LowLevel as B

type OperationVar = TVar (S.Set Operation)

data Master = Master { opVar    :: OperationVar
                     , thread   :: ThreadId
                     , stats    :: TVar Stats
                     }

data Stats = Stats { writes   :: Int
                   , retries  :: Int
                   , fails    :: Int
                   } deriving (Show, Eq, Ord)

data Operation = Operation { slave   :: Int
                           , command :: Command
                           } deriving (Show, Eq, Ord)

data Command = ReadInputBits { addr            :: Int
                             , nb              :: Int
                             , readInputBitsCb :: Callback [Bool]
                             }
             | WriteBit      { addr            :: Int
                             , status          :: Bool
                             , actionCb        :: Callback ()
                             } deriving (Show, Eq, Ord)

data Callback x = Callback (TMVar x)

instance (Show a) => Show (Callback a) where
  show _ = "<callback>"

instance (Eq a) => Eq (Callback a) where
  _ == _ = True

instance (Ord a) => Ord (Callback a) where
  compare _ _ = EQ

-- |This workaround returns equal element from the set if there is
-- any. The point is to return an element compares equal but is not
-- equal in practice.
setLookupEQ :: Ord a => a -> S.Set a -> Maybe a
setLookupEQ a s = case S.lookupGE a s of
  Just b -> if compare a b == EQ
            then Just b
            else Nothing
  Nothing -> Nothing

-- |Unwrap callback
unwrapCb :: Callback t -> TMVar t
unwrapCb (Callback a) = a

-- |Insert query to the queue. If there is such element, don't create
-- a new object but reuse the same variable.
enqueue :: Master -> (Command -> Callback a) -> (Callback a -> Operation) -> STM (STM a)
enqueue Master{..} getter opProto = do
  s <- readTVar opVar
  newOperation <- (opProto . Callback) <$> newEmptyTMVar
  case setLookupEQ newOperation s of
    Just oldOperation -> return $ readCallback oldOperation
    Nothing -> do
      writeTVar opVar $ S.insert newOperation s
      return $ readCallback newOperation
  where readCallback = readTMVar . unwrapCb . getter . command

-- |Take next element from the map. Retry when empty.
takeNext :: OperationVar -> TVar (Maybe Operation) -> STM Operation
takeNext opVar prevVar = do
  s <- readTVar opVar
  when (S.null s) retry
  mbPrev <- readTVar prevVar
  let next = case (mbPrev >>= flip S.lookupGT s) of
        Just x -> x
        Nothing -> S.findMin s
  writeTVar opVar $ S.delete next s
  writeTVar prevVar $ Just next
  return next

-- |Internally handle a single Modbus operation
handleModbus :: B.ModbusHandle -> TVar Stats -> Operation -> IO ()
handleModbus h statsVar Operation{..} = do
  B.setSlave h slave
  case command of
    ReadInputBits{..} -> wrap readInputBitsCb $ B.readInputBits h addr nb
    WriteBit{..} -> wrap actionCb $ B.writeBit h addr status
  where
    wrap callback act = catch (retryAction retryMax act >>= call callback . Right) (errHandle callback)
    errHandle callback e = call callback $ Left e
    call callback out = atomically $ do
      let put = putTMVar (unwrapCb callback)
      case out of
        Right (a, left) -> do
          Stats{..} <- readTVar statsVar
          writeTVar statsVar $ Stats{ writes  = succ writes
                                    , retries = retries + retryMax - left
                                    , fails   = fails
                                    }
          put a
        Left e -> do
          Stats{..} <- readTVar statsVar
          writeTVar statsVar $ Stats{ writes  = succ writes
                                    , retries = retries + retryMax
                                    , fails   = fails + 1
                                    }
          put $ throw (e :: B.ModbusException)
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
runMaster context = do
  opVar <- newTVarIO S.empty
  prevVar <- newTVarIO Nothing
  stats <- newTVarIO $ Stats 0 0 0
  thread <- forkIO $ forever $ do
    task <- atomically (takeNext opVar prevVar)
    handleModbus context stats task
    threadDelay 5000
  return Master{..}

readInputBits :: Master -> Int -> Int -> Int -> STM (STM [Bool])
readInputBits master slave addr nb = enqueue master readInputBitsCb $ Operation slave . ReadInputBits addr nb

writeBit :: Master -> Int -> Int -> Bool -> STM (STM ())
writeBit master slave addr status = enqueue master actionCb $ Operation slave . WriteBit addr status

-- |Run action synchronously.
sync :: STM (STM a) -> IO a
sync act = atomically act >>= atomically

-- |Read statistics
getStats :: Master -> STM Stats
getStats Master{..} = readTVar stats
