{-# LANGUAGE RecordWildCards #-}
module ActivityDetect where

import System.Process
import Control.Concurrent.STM
import Data.ByteString (hGetSome)
import System.Timeout (timeout)
import Control.Concurrent (forkIO)
import System.IO (hSetBuffering, BufferMode(NoBuffering))
import Data.Maybe (isJust)
import Control.Monad (forever)

activityDetect :: Int -> String -> IO (ProcessHandle, STM (Maybe Bool))
activityDetect holdTime cmd = do
  (_, Just h, _, procH) <- createProcess (shell cmd){ std_in = NoStream
                                                    , std_out = CreatePipe
                                                    }
  hSetBuffering h NoBuffering
  var <- newTVarIO Nothing
  forkIO $ forever $ do
    a <- timeout holdTime $ hGetSome h 1024
    atomically $ writeTVar var $ Just $ isJust a
  return (procH, readTVar var)


