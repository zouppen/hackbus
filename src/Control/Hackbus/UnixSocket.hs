{-# LANGUAGE OverloadedStrings #-}
module Control.Hackbus.UnixSocket where

import Control.Concurrent (forkFinally)
import Control.Exception (bracket, finally)
import Control.Monad (when, forever)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Network.Socket
import System.IO (Handle, IOMode(..), hClose)
import System.Posix.Files
import System.Directory (doesFileExist)
import System.Timeout (timeout)

type LineAction = BL.ByteString -> IO BL.ByteString 

-- |Copies file mode and ownership from another file.
copyPermissions :: FilePath -> FileStatus -> IO ()
copyPermissions f s = do
  setFileMode f (fileMode s)
  setOwnerAndGroup f (fileOwner s) (fileGroup s)

-- |Removes old dangling socket if it exists. Returns its status.
clearDanglingSocket :: FilePath -> IO (Maybe FileStatus)
clearDanglingSocket path = do
  exists <- doesFileExist path
  if exists
    then do status <- getFileStatus path
            if isSocket status
              then removeLink path >> pure (pure status)
              else fail "Socket path is a file"
    else pure Nothing

-- |Listen to new UNIX socket and fork a new thread per connection.
listenUnixSocket :: (Handle -> IO ()) -> FilePath -> IO ()
listenUnixSocket handler path = bracket open close loop
  where
    open = do
      mbStatus <- clearDanglingSocket path
      sock <- socket AF_UNIX Stream defaultProtocol
      bind sock $ SockAddrUnix path
      -- Copy old socket file permissions after the socket has created
      -- but before starting to accept connections.
      maybe (pure ()) (copyPermissions path) mbStatus
      listen sock 2
      return sock
    loop sock = forever $ do
      (conn, _) <- accept sock
      h <- socketToHandle conn ReadWriteMode
      forkFinally (handler h) $ const $ hClose h

-- |Single line chat. Per one incoming line there is always one
-- response line.
lineHandler :: LineAction -> Handle -> IO ()
lineHandler act handle = forever $ do
  line <- BL.fromStrict <$> BS.hGetLine handle
  ans <- act line
  BL.hPut handle $ ans `BL.snoc` 10

-- |Read data infinitely and never write. When data arrives, notify
-- function is called with True. If data flow stops for a given time
-- in microseconds, notify function is called with False.
activityDetect triggerDelay notify h = forever $ do
  a <- timeout triggerDelay $ BS.hGetSome h 1024
  notify $ case a of
    Just a -> if BS.null a then error "Socket disappeared" else True
    Nothing -> False

-- |Connect to pre-existing UNIX socket and process input with a function.
connectUnixSocket :: (Handle -> IO ()) -> String -> IO ()
connectUnixSocket handler path = do
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock $ SockAddrUnix path
  h <- socketToHandle sock ReadWriteMode
  finally (handler h) (hClose h)
