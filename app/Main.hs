module Main where

import MyMonoid

x = mappend' (1 :: Int) (2 :: Int)
y = mempty' :: Int
z = mappend' x y

a = mappend (1::Int) (2::Int)

main :: IO ()
main =  do print x
           print y
           print z
