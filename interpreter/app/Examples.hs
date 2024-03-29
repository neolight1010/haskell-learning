{-# LANGUAGE OverloadedStrings #-}

module Examples where

import Main (Expr (..), Defn (..), eval, M, Value (..))
import Parser (parseFun)

sumExpr :: Expr -> Expr
sumExpr =
  Let
    ( Rec
        "sum"
        ( Lambda
            ["n"]
            ( If
                (Equals (Var "n") (Number 0))
                (Number 0)
                ( Plus
                    (Var "n")
                    ( Apply
                        (Var "sum")
                        [Minus (Var "n") (Number 1)]
                    )
                )
            )
        )
    )

sumResult :: M Value
sumResult = eval [] (sumExpr (Apply (Var "sum") [Number 3]))

invalidResult :: M Value
invalidResult = eval [] (Plus (Number 5) (Boolean True))

monadicE :: Expr
monadicE = Let ((Val "x") New) (Seq (Assign (Var "x") (Number 42)) (Assign (Var "x") (Plus (Deref (Var "x")) (Number 1))))

monadicRes :: M Value
monadicRes = eval [] monadicE -- Use runState to run this in a Mem state.

parsedExpr :: Either String Expr
parsedExpr = parseFun "let rec fib = \\n -> if n == 0 then 0 else if n == 1 then 1 else %{fib: n - 1} + % {fib: n - 2} in %{fib: 7}"
