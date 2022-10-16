module SafeDivide where

import Failure (Failure (Ok, Fail))
import MyMonad (MyMonad (bind', return'))

safeDivide :: Failure Int -> Failure Int -> Failure Int
safeDivide xm ym =
  bind'
    xm
    ( \x ->
        bind' ym (\y -> if y == 0 then Fail else return' (x `div` y))
    )

{-- No Monad safeDivide 
 -
safeDivide :: MaybeError Int -> MaybeError Int -> MaybeError Int
safeDivide Error _ = Error
safeDivide _ Error = Error
safeDivide _ (Ok 0) = Error
safeDivide (Ok a) (Ok b) = Ok (a `div` b)
--}
