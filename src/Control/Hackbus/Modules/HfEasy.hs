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
import Control.Exception (handle)
import Control.Monad
import Control.Monad.Loops (iterateM_)
import Data.Aeson.Types
import Data.Maybe (isJust)
import Data.Text (Text)
import Network.Curl.Aeson
import Network.Curl.Opts
import Control.Hackbus.PeekPoke

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
-- *delay*. If action returns Nothing it means we are happy to wait
-- the whole delay (it's read action).
waitChange :: Eq a => Int -> Maybe a -> STM (Maybe a) -> IO (Maybe a)
waitChange delay old act = do
  timeoutVar <- registerDelay delay
  atomically $ do
    hasTimeout <- readTVar timeoutVar
    new <- act
    if hasTimeout || (isJust new && old /= new)
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
    angry <- handle (curlErrorConst False) $ case command of
      Nothing -> do
        state <- readRelay baseUrl
        atomically $ writeTVar stateVar $ Just state
        pure False
      Just targetState -> do
        -- Set relay state and verify it after a delay
        setRelay baseUrl targetState
        threadDelay 300000
        state <- readRelay baseUrl
        -- Then decide what we need to do
        atomically $ do
          writeTVar stateVar $ Just state
          -- We spent some time in IO operations so we need to recheck if the target has changed
          freshCommand <- readTVar commandVar
          if (freshCommand == Just state)
            then do writeTVar commandVar Nothing -- Stop writing
                    pure False -- All fine
            else pure True -- Rewrite ASAP
    -- Now waiting for control change.
    (if angry then atomically else waitChange 2000000 command) (readTVar commandVar)
  return $ HfEasy commandVar (TReadable stateVar) tId

-- |Error handler which just returns predefined value in case of an
-- cURL exception.
curlErrorConst :: a -> CurlAesonException -> IO a
curlErrorConst = const . pure
