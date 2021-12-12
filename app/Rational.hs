module Rational (Q (..), simpQ) where

data Q = Q Integer Integer

-- TODO: Fix errors when dealing with negative numbers.

simpQ :: Q -> Q
simpQ (Q a b) = Q (a `div` m) (b `div` m) where
  m = gcd a b

instance Show Q where
  show (Q a b) = show a ++ "/" ++ show b

instance Eq Q where
  r1 == r2 = a1' == a2' && b1' == b2' where
    (Q a1' b1') = simpQ r1
    (Q a2' b2') = simpQ r2

addQ (Q n1 d1) (Q n2 d2) = simpQ $ Q (n1' + n2') m
  where m = lcm d1 d2
        n1' = (m `div` d1) * n1
        n2' = (m `div` d2) * n2

instance Num Q where
  (+) = addQ
  negate (Q n d) = Q (-n) d
  Q n1 d1 * Q n2 d2 = simpQ $ Q (n1 * n2) (d1 * d2)
  abs (Q n d) = Q (abs n) (abs d)
  signum (Q n d) = Q (signum n * signum d) 1
  fromInteger a = Q a 1
