module ErrorHandler (MaybeError (..)) where

-- Error handler
data MaybeError a = Error | Ok a deriving Show
