module System.Hardware.Modbus.Types where

import Control.Concurrent.STM

type Query a = STM (STM a)
type Control a = a -> Query ()

data Stats = Stats { writes   :: Int
                   , retries  :: Int
                   , fails    :: Int
                   } deriving (Show, Eq, Ord)
