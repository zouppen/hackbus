module Control.Hackbus.TickSource (Accumulator(..), newAccumulator) where

import Control.Monad
import Control.Concurrent.STM
import System.Mem.Weak (Weak, deRefWeak)
import GHC.Conc (unsafeIOToSTM)

data Accumulator a = Accumulator { accumulate     :: a -> STM ()
                                 , dupAccumulator :: STM (STM a)
                                 }

-- |Create a new accumulator with given accumulator function.
newAccumulator :: (a -> a -> a) -> STM (Accumulator a)
newAccumulator f = do
  listeners <- newTVar []
  pure $ Accumulator (accumulate' listeners f) (dupAccumulator' listeners)

-- | Make new accumulator by adding element to the list of listeners.
dupAccumulator' :: TVar [Weak (TMVar a)] -> STM (STM a)
dupAccumulator' listeners = do
  var <- newEmptyTMVar
  weakVar <- unsafeIOToSTM $ mkWeakTMVar var $ pure ()
  modifyTVar listeners (weakVar:)
  pure $ takeTMVar var

-- |Accumulate a value and garbage collect old elements from the list
-- of listeners as a side effect. That's why unsafeIOSTM is used.
accumulate' :: TVar [Weak (TMVar a)] -> (a -> a -> a) -> a -> STM ()
accumulate' listeners f new = do
  oldListeners <- readTVar listeners
  newListeners <- flip filterM oldListeners $ \weakListener -> do
    mbListener <- unsafeIOToSTM $ deRefWeak weakListener
    case mbListener of
      Nothing       -> pure False
      Just listener -> do
        -- The actual patty. Taking old value and using an accumulator
        -- function to update it in-place.
        old <- tryTakeTMVar listener
        putTMVar listener $ maybe new (f new) old
        pure True
  writeTVar listeners newListeners
