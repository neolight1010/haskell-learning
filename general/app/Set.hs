module Set (Set (..), foldSet, isInSet, isSubSet) where

data Set a = Empty | Sing a | Union (Set a) (Set a)

instance Eq a => Eq (Set a) where
  r == s = r `isSubSet` s && s `isSubSet` r

foldSet :: (a -> b) -> (b -> b -> b) -> b -> Set a -> b
foldSet sing union empty Empty = empty
foldSet sing union empty (Sing x) = sing x
foldSet sing union empty (Union x y) = foldSet sing union empty x `union` foldSet sing union empty y

isInSet :: Eq a => a -> Set a -> Bool
isInSet x = foldSet (==x) (||) False

-- | Given two sets, `a` and `b`, returns True if a is a subset of b.
isSubSet :: Eq a => Set a -> Set a -> Bool
isSubSet s1 s2 = foldSet (`isInSet` s2) (&&) True s1
