module RevString (RevString (..)) where

newtype RevString = RevString String

instance Show RevString where
  show (RevString s) = reverse s

