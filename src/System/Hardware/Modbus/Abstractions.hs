module System.Hardware.Modbus.Abstractions where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (catch, throw)
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

-- |Relay which is controlled via Modbus function code 0x05 (force
-- single coil) and needs to be refreshed every given microseconds.
bitRelayWithTimer :: Int -> STM Bool -> Master -> Int -> Int -> IO ThreadId
bitRelayWithTimer timeout source master slave addr = do
  let loop state = do
        sync $ writeBit master slave addr state
        f <- if state
             then onKeeper <$> registerDelay timeout
             else return offKeeper
        atomically (f source) >>= loop
  atomically source >>= forkIO . loop

inputBitsVar :: Int -> Master -> Int -> Int -> Int -> IO ([STM Bool])
inputBitsVar interval master slave addr nb = do
  var <- action >>= mapM newTVarIO
  forkIO $ forever $ do
    threadDelay interval
    -- Try to divide current state to all variables. In case of an
    -- exception, propagate it to them.
    catch
      (action >>= atomically . sequence_ . zipWith writeTVar var)
      (\e -> atomically $ mapM_ (flip writeTVar (throw (e :: ModbusException))) var)
  return $ map readTVar var
  where action = sync $ readInputBits master slave addr nb

-- TODO how to kill these?
-- TODO split timer functionality from bitRelayWithTimer to allow to use it with other kind of relays as well
