{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

main :: IO ()
main = putStrLn "Hello, Haskell!"

data (a :: k) :> (b :: Type)
infixr 9 :>

data a :<|> b = a :<|> b
infixr 8 :<|>

data Capture (a :: Type)

data Result (a :: Type)

class HasClient api where
  type Client (api :: Type) :: Type

  client :: Proxy api -> [String] -> Client api

class HasServer api where
  type Server (api :: Type) :: Type

  route :: Proxy api -> Server api -> [String] -> Maybe (IO String)

class Capturable a where
  toMsg :: a -> String

instance (HasClient a, HasClient b) => HasClient (a :<|> b) where
  type Client (a :<|> b) = Client a :<|> Client b

  client _ x = client (Proxy @a) x :<|> client (Proxy @b) x

instance (Capturable a, HasClient b) => HasClient (Capture a :> b) where
  type Client (Capture a :> b) = a -> Client b

  client _  x z = client (Proxy @b) $ (toMsg z):x

instance (KnownSymbol s, HasClient b) => HasClient (s :> b) where
  type Client ((s :: Symbol) :> b) = Client b

  client _ x = client (Proxy @b) $ symb:x
    where
      symb = symbolVal (Proxy @s)
