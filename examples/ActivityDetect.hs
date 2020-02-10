{-# LANGUAGE RecordWildCards #-}
module ActivityDetect where

import System.IO
import System.Process
import Control.Concurrent.STM
import qualified Data.ByteString as B
import System.Timeout (timeout)
import Control.Concurrent (forkIO)
import System.IO (hSetBuffering, BufferMode(NoBuffering))
import Data.Maybe (isJust)
import Control.Monad (forever)

activityDetect :: Int -> Handle -> IO (STM (Maybe Bool))
activityDetect holdTime h = do
  hSetBuffering h NoBuffering
  var <- newTVarIO Nothing
  forkIO $ forever $ do
    a <- timeout holdTime $ B.hGetSome h 1024
    let ret = case a of
          Just a -> if B.null a then Just (error "Socket disappeared") else Just True
          Nothing -> Just False
    atomically $ writeTVar var ret
  return $ readTVar var


