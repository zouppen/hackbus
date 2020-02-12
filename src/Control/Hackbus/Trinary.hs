module Control.Hackbus.Trinary where

import Control.Monad
import Control.Applicative

-- Kleene logic
-- https://en.wikipedia.org/wiki/Three-valued_logic

infixr 3 <&&>
infixr 2 <||>

optional2 :: (Monad m, Alternative m) => (Maybe a -> Maybe b -> m c) -> m a -> m b -> m c
optional2 f a b = do
  ma <- optional a
  mb <- optional b
  f ma mb

-- |Trinary AND
(<&&>) :: (Monad m, Alternative m) => m Bool -> m Bool -> m Bool
(<&&>) = optional2 $ \a b -> case (a, b) of
  (Just False, _)          -> pure False
  (_, Just False)          -> pure False
  (Just True, Just True)   -> pure True
  _                        -> empty

-- |Trinary OR
(<||>) :: (Monad m, Alternative m) => m Bool -> m Bool -> m Bool
(<||>) = optional2 $ \a b -> case (a, b) of
  (Just True, _)           -> pure True
  (_, Just True)           -> pure True
  (Just False, Just False) -> pure False
  _                        -> empty

-- |Trinary NOT
fnot :: Functor f => f Bool -> f Bool
fnot = fmap not
