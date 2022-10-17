module Main where

type Id = String

data Expr
  = Number Int
  | Plus Expr Expr
  | Minus Expr Expr
  | Var Id
  | Let Id Expr Expr
  | Lambda [Id] Expr
  | Apply Expr [Expr]
  deriving (Show)

data Value
  = NumVal Int
  | Closure Env [Id] Expr
  deriving (Show)

type Env = [(Id, Value)]

eval :: Env -> Expr -> Value
eval _ (Number i) = NumVal i
eval env (Plus e1 e2) = NumVal $ v1 + v2
  where
    e1' = eval env e1
    e2' = eval env e2

    v1 = numOrError e1'
    v2 = numOrError e2'

    numOrError v = case v of
      NumVal x -> x
      Closure {} -> error closureError

    closureError = "Closure cannot be summed."
eval env (Minus e1 e2) = NumVal $ v1 - v2
  where
    e1' = eval env e1
    e2' = eval env e2

    v1 = numOrError e1'
    v2 = numOrError e2'

    numOrError v = case v of
      NumVal x -> x
      Closure {} -> error closureError

    closureError = "Closure cannot be substracted."
eval env (Var id') = find env id'
eval env (Let id' e1 e2) = eval (elab id' e1 env) e2
eval env (Lambda ids e) = Closure env ids e
eval env (Apply f exprs) = apply closure args
  where
    closure = eval env f
    args = map (eval env) exprs

apply :: Value -> [Value] -> Value
apply (Closure env ids expr) vals = eval (zip ids vals ++ env) expr
apply _ _ = error "Given value is not a function."

find :: Env -> Id -> Value
find env id' = snd . head . filter ((== id') . fst) $ env

elab :: Id -> Expr -> Env -> Env
elab id' expr env = (id', eval env expr) : env

main :: IO ()
main = putStrLn "Hello, Haskell!"
