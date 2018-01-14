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

-- |Poll single Modbus source
poll :: Int -> STM (STM a) -> IO (STM a, ThreadId)
poll interval get = do
  var <- sync get >>= newTVarIO
  tid <- forkIO $ forever $ do
    threadDelay interval
    act <- atomically get -- Send request
    atomically $ act >>= writeTVar var -- Collect response
  return (readTVar var, tid)

-- |Poll a Modbus source producing a list and output to separate
-- variables.
pollMany :: Int -> STM (STM [a]) -> IO ([STM a], ThreadId)
pollMany interval get = do
  vars <- sync get >>= mapM newTVarIO
  tid <- forkIO $ forever $ do
    threadDelay interval
    -- Try to divide current state to all variables. In case of an
    -- exception, propagate it to them.
    handle
      -- Fill with exceptions in case of an error
      (\e -> atomically $ mapM_ (\var -> writeTVar var $ throw (e :: ModbusException)) vars)
      -- Fill with ouputs in sucessfull case
      $ do
        act <- atomically get
        atomically $ act >>= sequence_ . zipWith writeTVar vars
  return (map readTVar vars, tid)

