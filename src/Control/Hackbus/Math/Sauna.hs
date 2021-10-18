{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- |Sauna data collection
module Control.Hackbus.Math.Sauna where

import Control.Applicative
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

-- |Sauna state machine. The thresholds are configurable. It is not
-- possible to go to Active state from Off state to avoid inadvertent
-- state change by very humid but cold air.
evalSauna :: SaunaConf -> Double -> Double -> SaunaState -> SaunaState
evalSauna SaunaConf{..} temp humi oldState = maybe oldState id $
  SaunaOff `at` (temp <= tempCooldown) <|>
  case oldState of
    SaunaOff     -> SaunaHeating `at` (temp >= tempHeating) 
    SaunaHeating -> SaunaReady `at` (temp >= tempReady) <|>
                    SaunaActive `at` (dp >= dpActive)
    SaunaReady   -> SaunaActive `at` (dp >= dpActive)
    _            -> empty
  where
    -- When getting invalid values from humidity, it's safest to
    -- assume very low dew point (causes no SaunaActive triggering)
    dp = if humi > 100 then -300 else dewPoint temp humi

-- |Returns first argument when second argument is true, otherwise
-- empty. Useful when wrapping with Maybe or another Alternative type.
at :: Alternative f => a -> Bool -> f a
at val True = pure val
at _ _      = empty
