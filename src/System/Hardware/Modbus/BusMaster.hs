module System.Hardware.Modbus.BusMaster where

import qualified System.Hardware.Modbus as B
import Control.Concurrent.STM
import Control.Monad (when)
import qualified Data.Map.Strict as M

type Callback x = x -> STM()
type OperationMap = M.Map Operation Response

{-
instance (Show a) => Show (Callback a) where
  show _ = "<callback>"

instance (Eq a) => Eq (Callback a) where
  _ == _ = True

instance (Ord a) => Ord (Callback a) where
  compare _ _ = EQ
-}

data Operation = Operation { slave   :: Int
                           , command :: Command
                           } deriving (Show, Eq, Ord)

data Command = ReadInputBits { addr        :: Int
                             , nb          :: Int
                             }
             | WriteBit      { addr        :: Int
                             , status      :: Bool
                             } deriving (Show, Eq, Ord)

data Response = ReadInputBitsResponse (Callback [Bool])
              | ActionResponse (Callback ())

-- |Insert query to the queue
enqueue :: TVar OperationMap -> Operation -> Response -> STM ()
enqueue mv command response = modifyTVar' mv $ M.insertWith mergeResponse command response

-- |Take next element from the map. Retry when empty.
takeNext :: TVar OperationMap -> Operation -> STM (Operation, Response)
takeNext mv prev = do
  m <- readTVar mv
  when (M.null m) retry
  let next@(k,_) = case M.lookupGT prev m of
        Nothing -> M.findMin m  
        Just x -> x
  writeTVar mv $ M.delete k m
  return next

combine :: Callback t -> Callback t -> Callback t
combine cb1 cb2 x = cb1 x >> cb2 x

mergeResponse (ReadInputBitsResponse a) (ReadInputBitsResponse b) = ReadInputBitsResponse $ combine a b
mergeResponse (ActionResponse a) (ActionResponse b) = ActionResponse $ combine a b
mergeResponse _ _ = error "Incompatible types in mergeResponse. Shouldn't happen!"

-- Public parts
runMaster context = do
  set <- newTVarIO
  return ()

-- TODO Muuta niin että käyttää samaa STM-actionia jos sellainen on jo jonossa ja readTMVar

wrap master operation response = do
  resp <- newEmptyTMVar
  enqueue master operation (response $ putTMVar resp)
  return $ takeTMVar resp

readInputBits master slave addr nb = wrap master (Operation slave $ ReadInputBits addr nb) ReadInputBitsResponse

