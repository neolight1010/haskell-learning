module MyFunctor (MyFunctor (..)) where

class MyFunctor f where
  fmap' :: (a -> b) -> f a -> f b

instance MyFunctor [] where
  fmap' f (x : xs) = f x : fmap' f xs
  fmap' _ [] = []
