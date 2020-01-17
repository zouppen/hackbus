module System.Hardware.Modbus.Abstractions where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (handle, throw)
import Control.Monad (forever, unless, when, zipWithM_)
import Control.Monad.Loops (iterateM_)

import System.Hardware.Modbus.Types

-- |Makes any output controllable via STM variable. State is refreshed
-- every given microseconds.
wireWithRefresh :: Eq a => Int -> STM a -> Control a -> IO ThreadId
wireWithRefresh timeout source control = forkIO $ flip iterateM_ (const False) $ \same -> do
  wait <- readTVar <$> registerDelay timeout
  (state, cb) <- atomically $ do
    state <- source
    when (same state) $ wait >>= check
    cb <- control state
    return (state, cb)
  atomically cb
  return (state ==)

-- |Poll single Modbus source periodically
pollRaw :: Int -> Query a -> (a -> STM ()) -> IO ThreadId
pollRaw interval query f = forkIO $ forever $ do
  threadDelay interval
  ans <- atomically query
  atomically $ ans >>= f

pollWithInterval :: Int -> Query a -> IO (STM a, ThreadId)
pollWithInterval interval query = do
  -- Query once, then start polling
  var <- sync query >>= newTVarIO
  tid <- pollRaw interval query $ writeTVar var
  return (readTVar var, tid)

pollManyWithInterval :: Int -> Query [a] -> IO ([STM a], ThreadId)
pollManyWithInterval interval query = do
  -- Query once, then start polling
  vars <- sync query >>= mapM newTVarIO
  tid <- pollRaw interval query $ zipWithM_ writeTVar vars
  return (map readTVar vars, tid)

-- |Poll given input every 100ms. Interval is sufficient for
-- iteractive things like wall switches.
poll :: Query a -> IO (STM a, ThreadId)
poll = pollWithInterval 100000

-- |Poll given input every 100ms and collect data as many STM
-- variables. Useful for getting multiple values from a single
-- register with only one read.
pollMany :: Query [a] -> IO ([STM a], ThreadId)
pollMany = pollManyWithInterval 100000

-- |Ordinary relay or other output. Refreshes state every 4 seconds.
wire :: Eq a => STM a -> Control a -> IO ThreadId
wire = wireWithRefresh 4000000

-- |Generic button which runs IO action every time a button is
-- pressed.
pushButton :: STM Bool -> IO () -> IO () -> IO ThreadId
pushButton source actOff actOn = do
  state <- atomically source
  forkIO $ handle state
  where handle True = do
          atomically $ source >>= check . not
          actOff
          handle False
        handle False = do
          atomically $ source >>= check
          actOn
          handle True

-- |Button which toggles a state when it is pressed once. If the switch is normally open (the usual case), pass True to `no`.
toggleButton :: Bool -> STM Bool -> TVar Bool -> IO ThreadId
toggleButton no source var = if no then act nop toggle else act toggle nop
  where act = pushButton source
        toggle = atomically $ modifyTVar var not

-- |Shorthand for doing nothing
nop :: IO ()
nop = return ()

-- |Run action synchronously.
sync :: Query a -> IO a
sync act = atomically act >>= atomically

-- |State machine for detecting load errors.
loadSense :: STM Bool -> STM Bool -> Int -> IO (STM Bool, ThreadId)
loadSense switch sense delay = do
  var <- newTVarIO False
  tid <- forkIO $ forever $ do
    -- State 1: Start counter when the switch is turned on
    atomically $ switch >>= check
    wait <- readTVar <$> registerDelay delay
    -- State 2: Let's see if the load follows control after delay
    bad <- atomically $ do
      stillOn <- switch
      if stillOn
        then do
          wait >>= check        -- Delay must elapse
          sense >>= check . not -- And load must fail
          writeTVar var True    -- Store state to var
          return True
        else return False       -- Switch is turned off
    -- State 3: Only if we failed. Wait until we recover
    when bad $ atomically $ do
      a <- switch
      b <- sense
      when (a /= b) retry       -- Load must match switch state
      writeTVar var False       -- We have recovered
  return (readTVar var, tid)
