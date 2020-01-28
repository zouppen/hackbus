module Control.Hackbus.ModuleUtils where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (handle, throw)
import Control.Monad (forever, unless, when, zipWithM_)
import Control.Monad.Loops (iterateM_)

import Control.Hackbus.Trinary (Input, get)

-- |Execute given STM action when another STM action changes state
wireSTM :: Eq a => Input a -> (a -> STM ()) -> IO ThreadId
wireSTM source control = forkIO $ flip iterateM_ (const False) $ \same -> atomically $ do
  state <- get source
  when (same state) retry
  -- State has changed (or first iteration)
  control state
  -- Return comparator
  pure (state ==)

-- |Execute given IO action when STM action changes
wireIO :: Eq a => Input a -> (a -> IO ()) -> IO ThreadId
wireIO source control = forkIO $ flip iterateM_ (const False) $ \same -> do
  state <- atomically $ do
    state <- get source
    when (same state) retry
    pure state
  -- State has changed (or first iteration)
  control state
  -- Return comparator
  pure (state ==)
