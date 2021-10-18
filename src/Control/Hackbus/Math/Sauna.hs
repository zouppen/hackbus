{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- |Sauna data collection
module Control.Hackbus.Math.Sauna where

import Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON)
import Control.Hackbus.Math.DewPoint

data SaunaState
  = SaunaOff      -- ^Sauna is currently cold (not heated)
  | SaunaHeating  -- ^Sauna is being heated
  | SaunaReady    -- ^Sauna has reached target temperature
  | SaunaActive   -- ^Löyly has been thown to kiuas.
  deriving (Eq, Show)

instance ToJSON SaunaState where
  toJSON SaunaOff     = "off"
  toJSON SaunaHeating = "heating"
  toJSON SaunaReady   = "ready"
  toJSON SaunaActive  = "active"

instance FromJSON SaunaState where
  parseJSON v = case v of
    "off"     -> pure SaunaOff
    "heating" -> pure SaunaHeating
    "ready"   -> pure SaunaReady
    "active"  -> pure SaunaActive
    _         -> error "Unknown SaunaState"

data SaunaConf = SaunaConf
  { tempCooldown :: Double -- ^Temperature when sauna is considered being cold
  , tempHeating  :: Double -- ^At this temperature the heating has begun
  , tempReady    :: Double -- ^Temperature when sauna is ready for use
  , dpActive     :: Double -- ^Dew point when it seems someone has started to bath
  } deriving (Show)

-- |Evaluate sauna conditions
evalSauna :: SaunaConf -> Double -> Double -> SaunaState -> SaunaState
evalSauna SaunaConf{..} temp humi oldState =
  if temp <= tempCooldown
  then SaunaOff -- Only route to get back to start state
  else case oldState of
    SaunaOff     -> if temp >= tempHeating
                    then SaunaHeating
                    else oldState
    SaunaHeating -> if temp >= tempReady
                    then SaunaReady
                    else oldState
    SaunaReady   -> if dp >= dpActive
                    then SaunaActive
                    else oldState
    _            -> oldState
  where
    -- When getting invalid values from humidity, it's safest to
    -- assume very low dew point (causes no SaunaActive triggering)
    dp = if humi > 100 then -300 else dewPoint temp humi
