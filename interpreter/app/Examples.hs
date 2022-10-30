module Examples where

import Main (Expr (..), Defn (..), eval, M, Value (..))

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
