module Main where

type Id = String

data Expr
  = Number Int
  | Plus Expr Expr
  | Minus Expr Expr
  | Var Id
  | Let Defn Expr
  | Lambda [Id] Expr
  | Apply Expr [Expr]
  deriving (Show)

data Value
  = NumVal Int
  | Closure Env [Id] Expr
  deriving (Show)

data Defn = Val Id Expr
          | Rec Id Expr
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

    closureError = "Only numbers can bu summed."
eval env (Minus e1 e2) = NumVal $ v1 - v2
  where
    e1' = eval env e1
    e2' = eval env e2

    v1 = numOrError e1'
    v2 = numOrError e2'

    numOrError v = case v of
      NumVal x -> x
      Closure {} -> error closureError

    closureError = "Only numbers can be substracted."
eval env (Var id') = find env id'
eval env (Let defn e2) = eval (elab defn env) e2
eval env (Lambda ids e) = Closure env ids e
eval env (Apply f exprs) = apply closure args
  where
    closure = eval env f
    args = map (eval env) exprs

apply :: Value -> [Value] -> Value
apply (Closure env ids expr) vals = eval (zip ids vals ++ env) expr
apply _ _ = error "Only functions can be evaluated."

find :: Env -> Id -> Value
find env id' = snd . head . filter ((== id') . fst) $ env

elab :: Defn -> Env -> Env
elab (Val id' expr) env = (id', eval env expr) : env
elab (Rec id' (Lambda args expr)) env = env' where env' = (id', Closure env' args expr):env
elab _ _ = error "Only lambdas can be recursive"

main :: IO ()
main = putStrLn "Hello, Haskell!"
