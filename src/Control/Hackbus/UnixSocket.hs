{-# LANGUAGE OverloadedStrings #-}
module Control.Hackbus.UnixSocket where

import Control.Concurrent (forkFinally)
import Control.Exception (bracket)
import Control.Monad (when, forever)
import qualified Data.ByteString as B (hGetLine)
import qualified Data.ByteString.Lazy as B
import Network.Socket
import System.IO (Handle, IOMode(..), hClose)
import System.Posix.Files
import System.Directory (doesFileExist)

type LineAction = B.ByteString -> IO B.ByteString 

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

lineHandler :: LineAction -> Handle -> IO ()
lineHandler act handle = forever $ do
  line <- B.fromStrict <$> B.hGetLine handle
  ans <- act line
  B.hPut handle $ ans `B.snoc` 10

connectUnixSocket :: FilePath -> IO Handle
connectUnixSocket path = do
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock $ SockAddrUnix path
  socketToHandle sock ReadWriteMode
