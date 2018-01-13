{-# LANGUAGE RecordWildCards #-}
module System.Hardware.Modbus.BusMaster where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (when)
import qualified Data.Set as S
import qualified System.Hardware.Modbus as B

type OperationSet = S.Set Operation

data Master = Master { opVar    :: TVar OperationSet
                     , thread   :: ThreadId
                     , prevVar  :: TVar Operation
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
  where unwrap (Callback a) = a
        readCallback = readTMVar . unwrap . getter . command

-- |Take next element from the map. Retry when empty.
takeNext :: Master -> STM Operation
takeNext Master{..} = do
  s <- readTVar opVar
  when (S.null s) retry
  prev <- readTVar prevVar
  let next = case S.lookupGT prev s of
        Nothing -> S.findMin s  
        Just x -> x
  writeTVar opVar $ S.delete next s
  return next

-- Public parts
runMaster context = do
  set <- newTVarIO
  return ()

readInputBits :: Master -> Int -> Int -> Int -> STM (STM [Bool])
readInputBits master slave addr nb = enqueue master readInputBitsCb $ Operation slave . ReadInputBits addr nb

writeBit :: Master -> Int -> Int -> Bool -> STM (STM ())
writeBit master slave addr status = enqueue master actionCb $ Operation slave . WriteBit addr status
