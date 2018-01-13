{-# LANGUAGE RecordWildCards #-}
module System.Hardware.Modbus.BusMaster where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (when)
import qualified Data.Set as S
import qualified System.Hardware.Modbus as B

type CommandSet = S.Set Command

data Master = Master { cmdVar   :: TVar CommandSet
                     , thread   :: ThreadId
                     }

data Command = ReadInputBits { slave       :: Int
                             , addr        :: Int
                             , nb          :: Int
                             , readInputBitsCb :: Callback [Bool]
                             }
             | WriteBit      { slave       :: Int
                             , addr        :: Int
                             , status      :: Bool
                             , actionCb    :: Callback ()
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
enqueue :: Master -> (Command -> Callback a) -> (Callback a -> Command) -> STM (STM a)
enqueue Master{..} getter commandProto = do
  s <- readTVar cmdVar
  newCommand <- (commandProto . Callback) <$> newEmptyTMVar
  case setLookupEQ newCommand s of
    Just oldCommand -> return $ readCallback oldCommand
    Nothing -> do
      writeTVar cmdVar $ S.insert newCommand s
      return $ readCallback newCommand
  where unwrap (Callback a) = a
        readCallback = readTMVar . unwrap . getter 

-- |Take next element from the map. Retry when empty.
takeNext :: TVar CommandSet -> Command -> STM Command
takeNext mv prev = do
  s <- readTVar mv
  when (S.null s) retry
  let next = case S.lookupGT prev s of
        Nothing -> S.findMin s  
        Just x -> x
  writeTVar mv $ S.delete next s
  return next

-- Public parts
runMaster context = do
  set <- newTVarIO
  return ()

readInputBits :: Master -> Int -> Int -> Int -> STM (STM [Bool])
readInputBits master slave addr nb = enqueue master readInputBitsCb $ ReadInputBits slave addr nb

writeBit :: Master -> Int -> Int -> Bool -> STM (STM ())
writeBit master slave addr status = enqueue master actionCb $ WriteBit slave addr status
