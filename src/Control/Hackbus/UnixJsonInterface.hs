module Control.Hackbus.UnixJsonInterface where

import Data.Aeson
import Data.Text (Text, unpack)
import Control.Hackbus.Exceptions
import Control.Hackbus.UnixSocket
import Control.Hackbus.JsonCommands
import Control.Hackbus.PeekPoke
import Control.Concurrent.STM
import Control.Exception
import qualified Data.HashMap.Strict as M

data Access = Access { reader :: Maybe (STM Value)
                     , writer :: Maybe (Value -> STM ())
                     }

listenJsonQueries :: M.HashMap Text Access -> FilePath -> IO ()
listenJsonQueries m path = listenUnixSocket (lineHandler $ handleQuery m) path

connectJsonQueries :: M.HashMap Text Access -> FilePath -> IO ()
connectJsonQueries m path = connectUnixSocket (lineHandler $ handleQuery m) path

-- |Process queries coming from the socket. TODO make this more flexible.
handleQuery :: M.HashMap Text Access -> LineAction
handleQuery m line = do
  ans <- handle exceptionToAnswer $ case eitherDecode line of
    Left e            -> throwIO $ HackbusNonfatalException  e
    Right (Read keys) -> do
      list <- atomically $ mapM readKey keys
      return $ Return $ M.fromList list
    Right (Write m) -> do
      atomically $ mapM writeKey $ M.toList m
      return Wrote
  return $ encode ans
  where
    readKey key = do
      f <- look reader key
      value <- f
      return (key, value)
    writeKey (key, value) = do
      f <- look writer key
      f value
    look :: (Access -> Maybe b) -> Text -> STM b
    look field k = case M.lookup k m of
      Nothing  -> throwSTM $ HackbusNonfatalException $ "Key not found: " ++ unpack k
      Just acc -> case field acc of
        Nothing -> throwSTM $ HackbusNonfatalException $ "Permission denied: " ++ unpack k
        Just f  -> return f

-- |Catches all exceptions and returns them to the client. NB! May
-- reveal internal implementation details.
exceptionToAnswer :: SomeException -> IO Answer
exceptionToAnswer e = return $ Failed $ show e

-- |Read only access to variable
readonly :: (Readable a, ToJSON b) => a b -> Access
readonly a = Access (Just (read' a)) Nothing

-- |Write only access to variable
writeonly :: (Writable a, FromJSON b) => a b -> Access
writeonly a = Access Nothing (Just (write' a))

-- |Random access to variable
readwrite :: (Readable a, Writable a, ToJSON b, FromJSON b) => a b -> Access
readwrite a = Access (Just (read' a)) (Just (write' a))

-- |Run any STM action, read not supported
action :: FromJSON a => (a -> STM ()) -> Access
action f = Access Nothing (Just (act' f))

-- |Run STM action. Unsafe in that sense the action may hide side effects
readAction :: ToJSON a => STM a -> Access
readAction f = Access (Just (readUnsafe' f)) Nothing
