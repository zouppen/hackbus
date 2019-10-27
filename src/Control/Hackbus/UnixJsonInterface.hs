module Control.Hackbus.UnixJsonInterface where

import Data.Aeson
import Data.Text (Text, unpack)
import Control.Hackbus.UnixSocket
import Control.Hackbus.JsonCommands
import Control.Concurrent.STM
import Control.Exception
import qualified Data.Map.Lazy as M

data Access = Access { reader :: Maybe (STM Value)
                     , writer :: Maybe (Value -> STM ())
                     }

listenJsonQueries :: M.Map Text Access -> FilePath -> IO ()
listenJsonQueries m path = listenUnixSocket (lineHandler $ handleQuery m) path

-- |Process queries coming from the socket. TODO make this more flexible.
handleQuery :: M.Map Text Access -> LineAction
handleQuery m line = do
  ans <- handle exceptionToAnswer $ case eitherDecode line of
    Left e            -> fail e
    Right (Read k)    -> do
      f <- look reader k
      Return <$> atomically f
    Right (Write k v) -> do
      f <- look writer k
      atomically (f v)
      return Wrote
  return $ encode ans
  where
    look :: (Monad m) => (Access -> Maybe b) -> Text -> m b
    look field k = case M.lookup k m of
      Nothing  -> fail $ "Key not found: " ++ unpack k
      Just acc -> case field acc of
        Nothing -> fail $ "Permission denied: " ++ unpack k
        Just f  -> return f

-- |Catches all exceptions and returns them to the client. NB! May
-- reveal internal implementation details.
exceptionToAnswer :: SomeException -> IO Answer
exceptionToAnswer e = return $ Failed $ show e

class Readable a where
  peek :: a b -> STM (Maybe b)

instance Readable TVar where
  peek v = Just <$> readTVar v

instance Readable TMVar where
  peek = tryReadTMVar

class Writable a where
  poke :: a b -> b -> STM ()

instance Writable TVar where
  poke = writeTVar

instance Writable TMVar where
  poke = putTMVar

read' :: (Readable a, ToJSON b) => a b -> STM Value
read' var = toJSON <$> peek var

readUnsafe' :: ToJSON a => STM a -> STM Value
readUnsafe' act = toJSON <$> act

write' :: (Writable a, FromJSON b) => a b -> Value -> STM ()
write' var = act' $ poke var

act' :: (FromJSON a) => (a -> STM ()) -> Value -> STM ()
act' f val = case fromJSON val of
  Success a -> f a
  Error e   -> fail e

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
