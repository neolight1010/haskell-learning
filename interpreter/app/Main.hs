{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (Expr (..), Value (..), Defn (..), eval, apply, main, M) where

type Ident = String

type M = Either String

data Expr
  = Number Int
  | Boolean Bool
  | Plus Expr Expr
  | Minus Expr Expr
  | Var Ident
  | Let Defn Expr
  | If Expr Expr Expr
  | Equals Expr Expr
  | Lambda [Ident] Expr
  | Apply Expr [Expr]
  deriving (Show, Eq)

data Value
  = NumVal Int
  | BoolVal Bool
  | Closure Env [Ident] Expr
  deriving (Show, Eq)

data Defn
  = Val Ident Expr
  | Rec Ident Expr
  deriving (Show, Eq)

type Env = [(Ident, Value)]

eval :: Env -> Expr -> M Value
eval _ (Number i) = pure $ NumVal i
eval env (Plus e1 e2) = do
  let parseNumVal e = eval env e >>= \e' -> case e' of
        NumVal v -> Right v
        _ -> Left "Only numbers can be summed."

  v1 <- parseNumVal e1
  v2 <- parseNumVal e2

  pure $ NumVal (v1 + v2)

eval env (Minus e1 e2) = do
  let parseNumVal e = eval env e >>= \e' -> case e' of
        NumVal v -> Right v
        _ -> Left "Only numbers can be subtracted."

  v1 <- parseNumVal e1
  v2 <- parseNumVal e2

  pure $ NumVal (v1 - v2)

eval _ (Boolean b) = pure $ BoolVal b
eval env (Var id') = pure $ find env id'
eval env (Let defn e2) = elab defn env >>= \env' -> eval env' e2
eval env (If g e1 e2) = eval env g >>= \r -> case r of
  BoolVal True -> eval env e1
  BoolVal False -> eval env e2
  _ -> error "Only booleans are allowed in if expressions"
eval env (Equals e1 e2) = BoolVal <$> ((==) <$> e1' <*> e2')
  where
    e1' = eval env e1
    e2' = eval env e2
eval env (Lambda ids e) = pure $ Closure env ids e
eval env (Apply f exprs) = do
  closure <- eval env f
  args <- mapM (eval env) exprs
  apply closure args

apply :: Value -> [Value] -> M Value
apply (Closure env ids expr) vals = eval (zip ids vals ++ env) expr
apply _ _ = Left "Only functions can be evaluated."

find :: Env -> Ident -> Value
find env id' = snd . head . filter ((== id') . fst) $ env

elab :: Defn -> Env -> M Env
elab (Val id' expr) env = do
  e' <- eval env expr
  pure $ (id', e'):env
elab (Rec id' (Lambda ids e)) env = pure env'
  where
    env' = (id', Closure env' ids e):env
elab _ _ = Left "Only lambdas can be recursive"

main :: IO ()
main = putStrLn "Hello, Haskell!"
