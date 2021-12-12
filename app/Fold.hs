module Fold (foldr') where

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' cons empty [] = empty
foldr' cons empty (x:xs) = x `cons` (foldr' cons empty xs)
