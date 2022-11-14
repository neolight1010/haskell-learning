{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (Expr (..), Value (..), Defn (..), eval, apply, main, M, State (runState)) where

data Id a = Id a

instance Functor Id where
  fmap f (Id x) = Id (f x)

instance Applicative Id where
  pure x = Id x
  (Id f) <*> (Id x) = Id (f x)

instance Monad Id where
  (Id x) >>= f = f x

-- State implementation

data State m a = State { runState :: m -> (a, m)}

instance Functor (State m) where
  fmap f (State fs) = State $
    \m -> let (a, m') = fs m
          in (f a, m')

instance Applicative (State m) where
  pure x = State $ \m -> (x, m)

  (State ffs) <*> (State fs) = State $ \m -> let (ffs', m') = ffs m in
    let (fs', m'') = fs m' in
    (ffs' fs', m'')

instance Monad (State m) where
  (State fs) >>= f = State $ \m -> let (x, m') = fs m in
    let (State g) = f x in
    g m'


get :: State m m
get = State $ \m -> (m, m)

set :: a -> State a ()
set x = State $ \_ -> ((), x)

-- Interpreter implementation

type Ident = String

type Mem = [Value]

type M a = State Mem a

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
  | New
  | Deref Expr
  | Seq Expr Expr
  | Assign Expr Expr
  deriving (Show, Eq)

data Value
  = NumVal Int
  | BoolVal Bool
  | Closure Env [Ident] Expr
  | Null
  | MemAddr Int
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
        NumVal v -> return v
        _ -> error "Only numbers can be summed."

  v1 <- parseNumVal e1
  v2 <- parseNumVal e2

  pure $ NumVal (v1 + v2)

eval env (Minus e1 e2) = do
  let parseNumVal e = eval env e >>= \e' -> case e' of
        NumVal v -> return v
        _ -> error "Only numbers can be subtracted."

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
eval env (Deref e) = do
  i <- eval env e >>= \e' -> case e' of
    MemAddr v -> return v
    _ -> error "Only numbers allowed in Deref."

  mem <- get

  return $ mem !! i
eval _ New = do
  mem <- get

  let ret = length mem
  set $ mem ++ [Null]

  return $ MemAddr ret
eval env (Seq e1 e2) = eval env e1 >> eval env e2
eval env (Assign e1 e2) = do
  i <- eval env e1 >>= \e1' -> case e1' of
    MemAddr i -> return i
    _ -> error "Only MemAddr allowed in Assign."
  e <- eval env e2

  mem <- get
  let mem' = take i mem ++ [e] ++ drop (i+1) mem

  set mem'

  return Null

apply :: Value -> [Value] -> M Value
apply (Closure env ids expr) vals = eval (zip ids vals ++ env) expr
apply _ _ = error "Only functions can be evaluated."

find :: Env -> Ident -> Value
find env id' = snd . head . filter ((== id') . fst) $ env

elab :: Defn -> Env -> M Env
elab (Val id' expr) env = do
  e' <- eval env expr
  pure $ (id', e'):env
elab (Rec id' (Lambda ids e)) env = pure env'
  where
    env' = (id', Closure env' ids e):env
elab _ _ = error "Only lambdas can be recursive"

main :: IO ()
main = putStrLn "Hello, Haskell!"
