module System.Hardware.Modbus.Abstractions where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (handle, throw)
import Control.Monad (forever, unless)
import System.Hardware.Modbus.Types

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

-- |Waits until input is true (e.g. button is pressed)
waitUntil :: Bool -> STM Bool -> STM ()
waitUntil state input = do
  now <- input
  unless (now == state) retry

-- |Generic button which runs IO action every time a button is
-- pressed.
pushButton :: STM Bool -> IO () -> IO () -> IO ThreadId
pushButton source actOff actOn = do
  state <- atomically source
  forkIO $ handle state
  where handle True = do
          atomically $ waitUntil False source
          actOff
          handle False
        handle False = do
          atomically $ waitUntil True source
          actOn
          handle True

-- |Button which toggles a state when it is pressed once.
toggleButton :: STM Bool -> TVar Bool -> IO ThreadId
toggleButton source var = pushButton source nop $ atomically $ modifyTVar var not

-- |Helper function for retreiving a single value from a query
item :: Functor f => f [a] -> Int -> f a
item list i = (!! i) <$> list

-- |Shorthand for doing nothing
nop :: IO ()
nop = return ()

-- |Run action synchronously.
sync :: STM (STM a) -> IO a
sync act = atomically act >>= atomically
