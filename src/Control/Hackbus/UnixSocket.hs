{-# LANGUAGE OverloadedStrings #-}
module Control.Hackbus.UnixSockets where

import Data.Aeson (eitherDecode, encode, toJSON)
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (when, forever)
import qualified Data.ByteString as B (hGetLine)
import qualified Data.ByteString.Lazy as B
import Network.Socket
import System.IO (IOMode(..), hClose)
import System.Posix.Files (isSocket, getFileStatus, removeLink)
import System.Directory (doesFileExist)

import Control.Hackbus.JsonCommands

-- |Removes old dangling socket if it exists
clearDanglingSocket :: FilePath -> IO ()
clearDanglingSocket path = do
  exists <- doesFileExist path
  when exists $ do
    status <- getFileStatus path
    if isSocket status
      then removeLink path
      else fail "Socket path is a file"

openUnixSocket path = do
  clearDanglingSocket path
  E.bracket open close loop
  where
    open = do
      sock <- socket AF_UNIX Stream defaultProtocol
      bind sock $ SockAddrUnix path
      listen sock 1024
      return sock
    loop sock = forever $ do
      (conn, _) <- accept sock
      h <- socketToHandle conn ReadWriteMode
      forkFinally (handleQueries undefined h) (const $ hClose h)

handleQueries m handle = forever $ do
  line <- B.fromStrict <$> B.hGetLine handle
  ans <- case eitherDecode line of
    Left e            -> return $ Failed e
    Right (Read k)    -> return $ Return $ toJSON (12::Int) -- MOCK
    Right (Write k v) -> return $ Wrote -- MOCK
  B.hPut handle $ encode ans
