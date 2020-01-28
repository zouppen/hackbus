module Control.Hackbus.ModuleUtils where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (handle, throw)
import Control.Monad (forever, unless, when, zipWithM_)

import Control.Hackbus.Trinary (Input, get)

data Equality a = Const Bool | Equals a
type StateVar a = TVar (Equality a)


stateChanger stateVar source = do
  oldState <- readTVar stateVar
  state <- get source
  writeTVar stateVar $ Equals state
  if isSame oldState state
    then retry
    else pure state
  where isSame old new = case old of
          Const  x -> x
          Equals x -> x == new

-- |Execute given STM action when another STM action changes state
wireSTM :: Eq a => StateVar a -> Input a -> (a -> STM ()) -> IO ThreadId
wireSTM stateVar source control =
  forkIO $
  forever $
  atomically $
  stateChanger stateVar source >>= control

-- |Execute given IO action when STM action changes
wireIO :: Eq a => StateVar a -> Input a -> (a -> IO ()) -> IO ThreadId
wireIO stateVar source control =
  forkIO $
  forever $
  (atomically $ stateChanger stateVar source) >>= control
