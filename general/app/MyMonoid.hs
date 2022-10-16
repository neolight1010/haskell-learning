module MyMonoid (Monoid' (..)) where

class Semigroup m => Monoid' m where
  mempty' :: m
  mappend' :: m -> m -> m
  mappend' = (<>)

instance Semigroup Int where
  a <> b = a + b

instance Monoid' Int where
  mempty' = 0

instance Monoid Int where
  mempty = 0
