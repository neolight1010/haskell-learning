module ErrorHandler (MaybeError (..), safeDivide) where

-- Error handler
data MaybeError a = Error | Ok a deriving Show

safeDivide :: MaybeError Int -> MaybeError Int -> MaybeError Int
safeDivide Error _ = Error
safeDivide _ Error = Error
safeDivide _ (Ok 0) = Error
safeDivide (Ok a) (Ok b) = Ok (a `div` b)
