-- |Exceptions used in Hackbus.
module Control.Hackbus.Exceptions where

import Control.Exception (Exception)

-- |We throw errors using this simple exception type
newtype HackbusFatalException = HackbusFatalException String deriving (Show)

instance Exception HackbusFatalException

-- |We throw less fatal using this simple exception type
newtype HackbusNonfatalException = HackbusNonfatalException String deriving (Show)

instance Exception HackbusNonfatalException
