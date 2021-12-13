module Failure where

import MyApplicative (MyApplicative (..))
import MyFunctor (MyFunctor (fmap'))
import MyMonad

data Failure a = Ok a | Fail deriving (Show)

instance MyFunctor Failure where
  fmap' f (Ok x) = Ok $ f x
  fmap' f Fail = Fail

instance MyApplicative Failure where
  pure' = Ok

  app' (Ok f) (Ok a) = Ok $ f a
  app' Fail _ = Fail
  app' _ Fail = Fail

instance MyMonad Failure where
  bind' (Ok a) f = f a
  bind' Fail _ = Fail
