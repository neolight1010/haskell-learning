module Main where

import SafeDivide (safeDivide)
import Failure

main :: IO ()
main = do
  print $ safeDivide (Ok 9)  (Ok 3)
  print $ safeDivide (Ok 5)  (Ok 0)
  print $ safeDivide Fail  (Ok 0)
  print $ safeDivide (Ok 4)  Fail
  print $ safeDivide Fail  Fail
