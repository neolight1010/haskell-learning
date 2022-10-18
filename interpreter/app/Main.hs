module Main (Expr (..), Value (..), Defn (..), eval, apply, main) where

type Id = String

data Expr
  = Number Int
  | Boolean Bool
  | Plus Expr Expr
  | Minus Expr Expr
  | Var Id
  | Let Defn Expr
  | If Expr Expr Expr
  | Equals Expr Expr
  | Lambda [Id] Expr
  | Apply Expr [Expr]
  deriving (Show, Eq)

data Value
  = NumVal Int
  | BoolVal Bool
  | Closure Env [Id] Expr
  deriving (Show, Eq)

data Defn
  = Val Id Expr
  | Rec Id Expr
  deriving (Show, Eq)

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
      _ -> error nonNumberError

    nonNumberError = "Only numbers can bu summed."
eval env (Minus e1 e2) = NumVal $ v1 - v2
  where
    e1' = eval env e1
    e2' = eval env e2

    v1 = numOrError e1'
    v2 = numOrError e2'

    numOrError v = case v of
      NumVal x -> x
      _ -> error nonNumberError

    nonNumberError = "Only numbers can be substracted."
eval _ (Boolean b) = BoolVal b
eval env (Var id') = find env id'
eval env (Let defn e2) = eval (elab defn env) e2
eval env (If g e1 e2) = case eval env g of
  BoolVal True -> eval env e1
  BoolVal False -> eval env e2
  _ -> error "Only booleans are allowed in if expressions"
eval env (Equals e1 e2) = BoolVal $ e1' == e2'
  where
    e1' = eval env e1
    e2' = eval env e2
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
elab (Rec id' (Lambda args expr)) env = env' where env' = (id', Closure env' args expr) : env
elab _ _ = error "Only lambdas can be recursive"

main :: IO ()
main = putStrLn "Hello, Haskell!"
