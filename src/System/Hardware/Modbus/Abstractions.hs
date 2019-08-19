module System.Hardware.Modbus.Abstractions where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (handle, throw)
import Control.Monad (forever, unless, when)
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
pollWithInterval interval query = do
  -- Query once, then loop
  var <- sync query >>= newTVarIO
  -- Actual loop
  tid <- forkIO $ forever $ do
    threadDelay interval
    ans <- atomically query
    atomically $ ans >>= writeTVar var
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

-- |Button which toggles a state when it is pressed once. If the switch is normally open (the usual case), pass True to `no`.
toggleButton :: Bool -> STM Bool -> TVar Bool -> IO ThreadId
toggleButton no source var = if no then act nop toggle else act toggle nop
  where act = pushButton source
        toggle = atomically $ modifyTVar var not

-- |Helper function for retreiving a single value from a query
item :: Functor f => f [a] -> Int -> f a
item list i = (!! i) <$> list

-- |Shorthand for doing nothing
nop :: IO ()
nop = return ()

-- |Run action synchronously.
sync :: STM (STM a) -> IO a
sync act = atomically act >>= atomically

-- |State machine for detecting load errors.
loadSense :: STM Bool -> STM Bool -> Int -> IO (STM Bool, ThreadId)
loadSense switch sense delay = do
  var <- newTVarIO False
  tid <- forkIO $ forever $ do
    -- Start counting from the moment switch is turned on
    atomically $ switch >>= check
    wait <- readTVar <$> registerDelay delay
    -- Let's see if we end up in a bad situation
    atomically $ do
      stillOn <- switch
      when stillOn $ do
        wait >>= check        -- Delay must elapse
        sense >>= check . not -- And load must fail
        writeTVar var True    -- Then we have an issue
    -- Wait until we recover from the situation
    atomically $ do
      a <- switch
      b <- sense
      when (a /= b) retry
      -- We have recovered
      writeTVar var False
  return (readTVar var, tid)
