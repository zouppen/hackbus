{-# LANGUAGE OverloadedStrings #-}
module Control.Hackbus.UnixSocket where

import Control.Concurrent (forkFinally)
import Control.Exception (bracket)
import Control.Monad (when, forever)
import qualified Data.ByteString as B (hGetLine)
import qualified Data.ByteString.Lazy as B
import Network.Socket
import System.IO (Handle, IOMode(..), hClose)
import System.Posix.Files (isSocket, getFileStatus, removeLink)
import System.Directory (doesFileExist)

type LineAction = B.ByteString -> IO B.ByteString 

-- |Removes old dangling socket if it exists
clearDanglingSocket :: FilePath -> IO ()
clearDanglingSocket path = do
  exists <- doesFileExist path
  when exists $ do
    status <- getFileStatus path
    if isSocket status
      then removeLink path
      else fail "Socket path is a file"

listenUnixSocket :: (Handle -> IO ()) -> FilePath -> IO ()
listenUnixSocket handler path = do
  clearDanglingSocket path
  bracket open close loop
  where
    open = do
      sock <- socket AF_UNIX Stream defaultProtocol
      bind sock $ SockAddrUnix path
      listen sock 1024
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
