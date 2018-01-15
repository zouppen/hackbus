module System.Hardware.Modbus.Abstractions where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (handle, throw)
import Control.Monad (forever)
import System.Hardware.Modbus

offKeeper :: STM Bool -> STM Bool
offKeeper source = do
  state <- source
  if state then return True else retry

onKeeper :: TVar Bool -> STM Bool -> STM Bool
onKeeper timer source = do
  state <- source
  timeout <- readTVar timer
  if state && not timeout then retry else return state

-- Public functions follow

-- |Wrapper which makes any relay controllable via STM variable and is
-- refreshed every given microseconds.
relayWithTimer :: Int -> STM Bool -> (Bool -> STM (STM ())) -> IO ThreadId
relayWithTimer timeout source control = do
  let loop state = do
        sync $ control state
        f <- if state
             then onKeeper <$> registerDelay timeout
             else return offKeeper
        atomically (f source) >>= loop
  atomically source >>= forkIO . loop

-- |Poll single Modbus source periodically
poll :: Int -> Call a -> IO (STM a, ThreadId)
poll interval get = do
  -- Initial value setup is a bit challenging
  var <- newTVarIO Nothing
  tid <- forkIO $ forever $ do
    threadDelay interval
    atomically $ get $ writeTVar var . Just
  return (readJust var, tid)
  where readJust var = do
          a <- readTVar var
          case a of
            Just a  -> return a
            Nothing -> retry
