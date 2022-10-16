module LinkedList (List (..), toHList, fromHList) where

data List a = Empty | Cons a (List a) deriving Show

toHList :: List a -> [a]

toHList Empty = []
toHList (Cons x xs) = x : toHList xs

fromHList :: [a] -> List a
fromHList [] = Empty
fromHList (x:xs) = Cons x (fromHList xs)
