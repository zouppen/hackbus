{-# LANGUAGE OverloadedStrings #-}
-- |Hackbus support for https://github.com/ljalves/hfeasy relay. You
-- need to first flash your device to use the custom firmware
-- available in that url. Supported devices include Verkkokauppa.com
-- Netwjork SP5 plus many other Ankuoo REC and Ankuoo NEO based
-- products.
--
-- The device has also a button which can control the state as
-- well. If command mode is Nothing, then the state of the button can
-- be read from state readable.
module Control.Hackbus.Modules.HfEasy (HfEasy(..), runHfEasyRelay) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Loops (iterateM_)
import Data.Aeson.Types
import Data.Text (Text)
import Network.Curl.Aeson
import Network.Curl.Opts
import Control.Hackbus.UnixJsonInterface

data HfEasy = HfEasy
  { command :: TVar (Maybe Bool) -- ^Control relay. Set to Nothing to let it be in any position.
  , state   :: TReadable Bool    -- ^Represents current state.
  , tId     :: ThreadId          -- ^Thread ID.
  }

opts :: [CurlOption]
opts = [CurlTimeout 3]

relayStatus :: Value -> Parser Bool
relayStatus (Object o) = (==("1" :: Text)) <$> (pure o..."relay_status")
relayStatus _ = mzero

-- |Gets relay current state.
readRelay :: String -> IO Bool
readRelay baseUrl = curlAeson relayStatus "GET" baseUrl opts noData

-- |Set relay state
setRelay :: String -> Bool -> IO Bool
setRelay baseUrl s = curlAeson relayStatus "GET" url opts noData
  where url = baseUrl ++ "?sw=" ++ if s then "1" else "0"

-- |Returns the value in var. If the value returned in *act* is the
-- same as in *old* wait for change to happen. Timeout is specified by
-- *delay*.
waitChange :: Eq a => Int -> a -> STM a -> IO a
waitChange delay old act = do
  timeoutVar <- registerDelay delay
  atomically $ do
    hasTimeout <- readTVar timeoutVar
    new <- act
    if hasTimeout || old /= new
      then pure new
      else retry

-- |Runs control loop
runHfEasyRelay :: String -> IO HfEasy
runHfEasyRelay baseUrl = do
  commandVar <- newTVarIO Nothing
  stateVar <- newTVarIO Nothing
  -- Loop, start by reading.
  tId <- forkIO $ flip iterateM_ Nothing $ \command -> do
    -- Run the command, either read or write
    state <- case command of
      Nothing    -> readRelay baseUrl
      Just state -> setRelay baseUrl state
    atomically $ writeTVar stateVar $ Just state
    -- Now waiting for control change. Refreshing the relay state
    -- periodically defined by the delay.
    waitChange 2000000 command $ readTVar commandVar
  return $ HfEasy commandVar (TReadable stateVar) tId
