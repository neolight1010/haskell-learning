module Main where

import Failure (Failure (Ok, Fail))
import MyMonad (MyMonad (bind', return'))

safeDivide :: Failure Int -> Failure Int -> Failure Int
safeDivide xm ym =
  bind'
    xm
    ( \x ->
        bind' ym (\y -> if y == 0 then Fail else return' (x `div` y))
    )

main :: IO ()
main = do
  print $ safeDivide (Ok 9)  (Ok 3)
  print $ safeDivide (Ok 5)  (Ok 0)
  print $ safeDivide Fail  (Ok 0)
  print $ safeDivide (Ok 4)  Fail
  print $ safeDivide Fail  Fail
