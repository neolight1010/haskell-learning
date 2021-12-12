module MyApplicative (MyApplicative (..)) where
import MyFunctor (MyFunctor)

class MyFunctor f => MyApplicative f where
  pure' :: a -> f a
  app' :: f (a -> b) -> f a -> f b

-- Example: fmap' Person (Ok "Anto") `app'` (Ok 1) `app'` (Ok (10, 10, 2002))
