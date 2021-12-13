module MyMonad where

import MyApplicative (MyApplicative (..))

class (MyApplicative m) => MyMonad m where
  return' :: a -> m a
  return' = pure'

  bind' :: m a -> (a -> m b) -> m b
