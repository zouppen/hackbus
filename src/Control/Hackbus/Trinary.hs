module Control.Hackbus.Trinary where

import Prelude as P
import Control.Monad
import Control.Concurrent.STM (STM, atomically, retry)

-- https://en.wikipedia.org/wiki/Three-valued_logic

type Input a = STM (Maybe a)

infixr 3 ∧
infixr 3 &&
infixr 2 ∨
infixr 2 ||

-- |Trinary AND
(∧) :: Input Bool -> Input Bool -> Input Bool
(∧) = liftM2 $ \a b -> case (a, b) of
  (Nothing, _)             -> Nothing
  (_, Nothing)             -> Nothing
  (Just True, Just True)   -> Just True
  _                        -> Just False

-- |Trinary OR
(∨) :: Input Bool -> Input Bool -> Input Bool
(∨) = liftM2 $ \a b -> case (a, b) of
  (Just True, _)           -> Just True
  (_, Just True)           -> Just True
  (Just False, Just False) -> Just False
  _                        -> Nothing

-- |Trinary NOT
not :: Input Bool -> Input Bool
not = (liftM . liftM) P.not

-- |Known value constant
known :: a -> Input a
known = (pure . pure)

-- |An unknown value constant
unknown :: Input a
unknown = pure Nothing

-- Aliases
(&&) = (∧)
(||) = (∨)

-- |Retry STM action until it is no longer Nothing
get :: Input a -> STM a
get var = var >>= maybe retry pure
