-- |Typeclasses Readable and Writable implementing peek and poke
-- functions and TReadable, which is read-only TVar.
module Control.Hackbus.PeekPoke
  ( Readable
  , Writable
  , TReadable
  , peekWithRetry
  , peek
  , poke
  , mkTReadable
  , mkTReadableWith
  ) where

import Control.Concurrent.STM

-- |Read only wrapper to TVar. Read with `peek`.
newtype TReadable a = TReadable (STM (Maybe a))

class Readable a where
  peek :: a b -> STM (Maybe b)

instance Readable TVar where
  peek v = Just <$> readTVar v

instance Readable TMVar where
  peek = tryReadTMVar

instance Readable TReadable where
  peek (TReadable act) = act

class Writable a where
  poke :: a b -> b -> STM ()

instance Writable TVar where
  poke = writeTVar

instance Writable TMVar where
  poke = putTMVar

-- |Read Readable type but retry if Nothing (data not yet
-- available). Note this won't empty TMVars, just peeks them.
peekWithRetry :: Readable a => a b -> STM b
peekWithRetry a = peek a >>= maybe retry pure

-- |Create TReadable from TVar.
mkTReadable :: TVar (Maybe a) -> TReadable a
mkTReadable = mkTReadableWith id

-- |Make TReadable from any TVar with a conversion function.
mkTReadableWith :: (a -> Maybe b) -> TVar a -> TReadable b
mkTReadableWith f a = TReadable $ f <$> readTVar a
