module Main where

import Control.Applicative (Alternative (..))

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (x', s') <- p s
    return (f x', s')

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)

  (Parser f) <*> (Parser x) = Parser $ \s -> do
    (f', s1) <- f s
    (x', s2) <- x s1
    return (f' x', s2)

instance Monad Parser where
  (Parser p) >>= f = Parser $ \s -> do
    (x', s') <- p s
    runParser (f x') s'
    
instance MonadFail Parser where
  fail _ = Parser $ const Nothing

instance Alternative Parser where
  empty = fail ""
  (Parser p1) <|> (Parser p2) = Parser $ \s ->
    case p1 s of
      Just x -> Just x
      Nothing -> p2 s

charParser :: Char -> Parser Char
charParser c = Parser charP
  where
    charP [] = Nothing
    charP (x:xs)
      | x == c = pure (c, xs)
      | otherwise = Nothing

stringParser :: String -> Parser String
stringParser = mapM charParser

spaceParser :: Parser Char
spaceParser =
  charParser ' ' <|>
  charParser '\n' <|>
  charParser '\t' <|>
  charParser '\r'

ss :: Parser [Char]
ss = many spaceParser

helloWorldParser :: Parser (String, String)
helloWorldParser =
  (,) <$>
    ( stringParser "Hello" <* ss
    )
  <*> stringParser "World"

intCharParser :: Parser Char
intCharParser =
  charParser '0' <|>
  charParser '1' <|>
  charParser '2' <|>
  charParser '3' <|>
  charParser '4' <|>
  charParser '5' <|>
  charParser '6' <|>
  charParser '7' <|>
  charParser '8' <|>
  charParser '9'

intParser :: Parser Int
intParser = read <$> some intCharParser

main :: IO ()
main = putStrLn "Hello, Haskell!"
