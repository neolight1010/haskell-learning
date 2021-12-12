module CharCounter (countChars) where

import Data.Char
import Data.List

countChars :: String -> [(Char, Int)]
countChars = display . group . sort. canonical

canonical :: String -> String
canonical = filter (/= ' ') . map normalize

normalize :: Char -> Char
normalize c | isUpper c = c
            | isLower c = toUpper c
            | otherwise = ' '

display :: [String] -> [(Char, Int)]
display = map (\x -> (head x, length x))
