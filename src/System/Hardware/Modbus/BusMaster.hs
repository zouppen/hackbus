{-# LANGUAGE RecordWildCards #-}
module System.Hardware.Modbus.BusMaster where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (when)
import qualified Data.Map.Strict as M
import qualified System.Hardware.Modbus as B

type CommandMap = M.Map Command Response

data Master = Master { opMapVar :: TVar CommandMap
                     , thread   :: ThreadId
                     }

data Command = ReadInputBits { slave       :: Int
                             , addr        :: Int
                             , nb          :: Int
                             }
             | WriteBit      { slave       :: Int
                             , addr        :: Int
                             , status      :: Bool
                             } deriving (Show, Eq, Ord)

data Response = ReadInputBitsResponse (TMVar [Bool])
              | ActionResponse (TMVar ())

-- |Insert query to the queue. If there is such element, don't create
-- a new object but reuse the same variable.
enqueue :: TVar CommandMap -> Command -> STM Response -> STM Response
enqueue mv command newResponse = do
  m <- readTVar mv
  case M.lookup command m of
    Just response -> return response
    Nothing       -> do
      response <- newResponse
      writeTVar mv $ M.insert command response m
      return response

-- |Take next element from the map. Retry when empty.
takeNext :: TVar CommandMap -> Command -> STM (Command, Response)
takeNext mv prev = do
  m <- readTVar mv
  when (M.null m) retry
  let next@(k,_) = case M.lookupGT prev m of
        Nothing -> M.findMin m  
        Just x -> x
  writeTVar mv $ M.delete k m
  return next

wrap :: Master -> Command -> (TMVar a -> Response) -> STM Response
wrap Master{..} operation response = enqueue opMapVar operation (response <$> newEmptyTMVar)

-- Public parts
runMaster context = do
  set <- newTVarIO
  return ()

readInputBits :: Master -> Int -> Int -> Int -> STM (STM [Bool])
readInputBits master slave addr nb = (\(ReadInputBitsResponse x) -> readTMVar x) <$> wrap master ReadInputBits{..} ReadInputBitsResponse

writeBit :: Master -> Int -> Int -> Bool -> STM (STM ())
writeBit master slave addr status = (\(ActionResponse x) -> readTMVar x) <$> wrap master WriteBit{..} ReadInputBitsResponse

