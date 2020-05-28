module Control.Hackbus.Logging where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import System.IO
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson (Value, ToJSON, toJSON, encode)
import Data.Text (Text)

import Control.Hackbus.JsonCommands
import Control.Hackbus.PeekPoke
import Control.Hackbus.UnixJsonInterface

-- |Watch named STM variables for changes
addWatches :: Traversable t => a -> t (a -> IO ()) -> IO (t ThreadId)
addWatches q = mapM $ \f -> forkIO $ f q

-- |Stop running watches
stopWatches :: Traversable t => t ThreadId -> IO ()
stopWatches = mapM_ killThread

-- |Action in which STM action is run first and it may return IO
-- action which is run soon after the transaction.
type HybridAction = STM (IO ())

-- |No IO action is performed
noIO :: HybridAction
noIO = pure $ pure ()

-- |Watch changes in a given STM variable. Watch is triggered every
-- time when value changes. Notify function is a hybrid action (has
-- both STM and IO actions). NB! If notify function retries it is not
-- called again until /source/ changes.
watchWithIO :: Eq a => STM a -> (a -> a -> HybridAction) -> IO ()
watchWithIO source notify = do
  oldVar <- atomically $ source >>= newTVar 
  forever $ join $ atomically $ do
    new <- source
    old <- readTVar oldVar
    when (old == new) retry
    writeTVar oldVar new
    notify old new `orElse` noIO

-- |Watch changes in a given STM variable. When value changes, run
-- given action. For a more generic version, see `watchWithIO`.
watch :: Eq v => STM v -> STM () -> IO ()
watch source notify = watchWithIO source (\_ _ -> notify >> noIO)

-- |JSON report formatter
jsonReport :: ToJSON a => Text -> a -> B.ByteString
jsonReport k v = encode $ Report k $ toJSON v

-- |Add element to queue with a key and a single value which is
-- printed only when it changes.
kv :: (Eq a, ToJSON a) => Text -> STM a -> TQueue B.ByteString -> IO ()
kv k v q = watch v $ do
  v' <- v
  writeTQueue q $ jsonReport k v'

-- |Add element to watch queue with a key and a value v which is
-- monitored for changes. When value stored in v changes, then it is
-- printed in addition to givon other values.
kvv :: (Readable a, Eq b, ToJSON b) => Text -> a b -> [STM Value] -> TQueue B.ByteString -> IO ()
kvv k v vs q = watch (peek v) $ do
  v'  <- read' v
  vs' <- sequence vs
  writeTQueue q $ jsonReport k (v':vs')

-- |Just a mnemonic for creating a new queue
newMonitorQueue :: IO (TQueue B.ByteString)
newMonitorQueue = newTQueueIO

-- |Run monitor which prints to given handle
runMonitor :: Handle -> TQueue B.ByteString -> IO b
runMonitor h q = forever $ atomically (readTQueue q) >>= act
  where act msg = B.hPut h $ B.snoc msg '\n'
