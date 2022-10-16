module Main where

type Id = String

data Expr
  = Number Int
  | Plus Expr Expr
  | Minus Expr Expr
  | Var Id
  | Let Id Expr Expr
  deriving (Show)

newtype Value = NumVal Int
  deriving (Show)

type Env = [(Id, Value)]

eval :: Env -> Expr -> Value
eval _ (Number i) = NumVal i
eval env (Plus e1 e2) = NumVal $ v1 + v2
  where
    NumVal v1 = eval env e1
    NumVal v2 = eval env e2
eval env (Minus e1 e2) = NumVal $ v1 - v2
  where
    NumVal v1 = eval env e1
    NumVal v2 = eval env e2
eval env (Var id') = find env id'
eval env (Let id' e1 e2) = eval (elab id' e1 env) e2

find :: Env -> Id -> Value
find env id' = snd . head . filter ((== id') . fst) $ env

elab :: Id -> Expr -> Env -> Env
elab id' expr env = (id', eval env expr) : env

main :: IO ()
main = putStrLn "Hello, Haskell!"
