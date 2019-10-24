module Control.Hackbus.UnixJsonInterface where

import Data.Aeson
import Data.Text (Text)
import Control.Hackbus.UnixSocket
import Control.Hackbus.JsonCommands
import Control.Concurrent.STM

data Access = Access { reader :: Maybe (STM Value)
                     , writer :: Maybe (Value -> STM ())
                     }

listenJsonQueries :: FilePath -> IO ()
listenJsonQueries = listenUnixSocket $ lineHandler $ handleQuery undefined

handleQuery :: t -> LineAction -- TODO mock
handleQuery m line = do
  ans <- case eitherDecode line of
    Left e            -> return $ Failed e
    Right (Read k)    -> return $ Return $ toJSON (12::Int) -- MOCK
    Right (Write k v) -> return $ Wrote -- MOCK
  return $ encode ans

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
