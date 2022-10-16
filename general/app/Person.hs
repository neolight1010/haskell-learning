module Person (Person (..)) where
import MyApplicative (MyApplicative)

-- Person
data Person = Person { name :: String,
                id   :: Int,
                dob  :: (Int, Int, Int)
              } deriving Show
