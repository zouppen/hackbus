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

-- |Wrapper which makes any relay controllable via STM variable. If
-- the relay is on the state is refreshed every given microseconds.
relayControlWithHold :: Int -> STM Bool -> Control Bool -> IO ThreadId
relayControlWithHold timeout source control = do
  let loop state = do
        sync $ control state
        f <- if state
             then onKeeper <$> registerDelay timeout
             else return offKeeper
        atomically (f source) >>= loop
  atomically source >>= forkIO . loop

-- |Poll single Modbus source periodically
pollWithInterval :: Int -> Query a -> IO (STM a, ThreadId)
pollWithInterval interval get = do
  -- Initial value setup is a bit challenging
  tmp <- newEmptyTMVarIO
  atomically $ get $ putTMVar tmp
  var <- atomically $ readTMVar tmp >>= newTVar
  -- Actual loop
  tid <- forkIO $ forever $ do
    threadDelay interval
    atomically $ get $ writeTVar var
  return (readTVar var, tid)

-- |Poll given input every 100ms. Useful interval for iteractive
-- things like wall switches.
poll :: Query a -> IO (STM a, ThreadId)
poll = pollWithInterval 100000

-- |Ordinary relay. Refreshes ON state every 4 seconds.

relayControl :: STM Bool -> Control Bool -> IO ThreadId
relayControl = relayControlWithHold 4000000

-- |Helper function for retreiving a single value from a query
item :: Functor f => f [a] -> Int -> f a
item list i = (!! i) <$> list
