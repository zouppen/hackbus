{-# LANGUAGE RecordWildCards #-}
module System.Hardware.Modbus
  ( B.Parity(..)
  , B.newRTU
  , B.connect
  , Master
  , runMaster
  , sync
  , readInputBits
  , writeBit
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, when)
import qualified Data.Set as S
import qualified System.Hardware.Modbus.LowLevel as B

type OperationVar = TVar (S.Set Operation)

data Master = Master { opVar    :: OperationVar
                     , thread   :: ThreadId
                     }

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
handleModbus :: B.ModbusHandle -> Operation -> IO ()
handleModbus h Operation{..} = do
  B.setSlave h slave
  case command of
    ReadInputBits{..} -> B.readInputBits h addr nb >>= call readInputBitsCb
    WriteBit{..} -> B.writeBit h addr status >>= call actionCb
  where
    call callback = atomically . putTMVar (unwrapCb callback)

-- Public parts

runMaster :: B.ModbusHandle -> IO Master
runMaster context = do
  opVar <- newTVarIO S.empty
  prevVar <- newTVarIO Nothing
  -- TODO implement resend logic and not die
  thread <- forkIO $ forever $ atomically (takeNext opVar prevVar) >>= handleModbus context
  return Master{..}

readInputBits :: Master -> Int -> Int -> Int -> STM (STM [Bool])
readInputBits master slave addr nb = enqueue master readInputBitsCb $ Operation slave . ReadInputBits addr nb

writeBit :: Master -> Int -> Int -> Bool -> STM (STM ())
writeBit master slave addr status = enqueue master actionCb $ Operation slave . WriteBit addr status

-- |Run action synchronously.
sync :: STM (STM a) -> IO a
sync act = atomically act >>= atomically
