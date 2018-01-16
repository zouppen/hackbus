module System.Hardware.Modbus.Types where

import Control.Concurrent.STM

type Callback a = a -> STM ()
type Query a = Callback a -> STM ()
type Control a = a -> STM (STM ())

data Stats = Stats { writes   :: Int
                   , retries  :: Int
                   , fails    :: Int
                   } deriving (Show, Eq, Ord)
