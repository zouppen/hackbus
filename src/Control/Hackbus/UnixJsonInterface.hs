module Control.Hackbus.UnixJsonInterface where

import Data.Aeson (eitherDecode, encode, toJSON)

import Control.Hackbus.UnixSocket
import Control.Hackbus.JsonCommands

listenJsonQueries :: FilePath -> IO ()
listenJsonQueries = listenUnixSocket $ lineHandler $ handleQuery undefined

handleQuery :: t -> LineAction -- TODO mock
handleQuery m line = do
  ans <- case eitherDecode line of
    Left e            -> return $ Failed e
    Right (Read k)    -> return $ Return $ toJSON (12::Int) -- MOCK
    Right (Write k v) -> return $ Wrote -- MOCK
  return $ encode ans
