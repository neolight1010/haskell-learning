module Examples where

import Main (Expr (..), Defn (..))

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
