{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}

module Parser (parseFun) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (char, decimal, many', many1, Parser, parseOnly, skipSpace, takeWhile1)
import Data.Text (Text)
import qualified Data.Text as T

import Main (Expr (..), Defn (..))

parseFun :: Text -> Either String Expr
parseFun t = parseOnly parseExpr t

parseExpr :: Parser Expr
parseExpr =
  parsePlusMinus <|> parseExpr'

parseExpr' :: Parser Expr
parseExpr' =
  parseConst <|>
  parseIf <|>
  parseLet <|>
  parseLam <|>
  parseApply <|>
  parseVar

parseConst :: Parser Expr
parseConst =
  Number <$> decimal <|>
  "True" *> pure (Boolean True) <|>
  "False" *> pure (Boolean False)

parseIf :: Parser Expr
parseIf = do
  "if" *> ss
  cond <- parseExpr <* ss
  "then" *> ss
  e1 <- parseExpr <* ss
  "else" *> ss
  e2 <- parseExpr <* ss

  return $ If cond e1 e2

parseVar :: Parser Expr
parseVar = Var <$> atom

parseLet :: Parser Expr
parseLet = do
  "let" *> ss
  d <- parseDefn <* ss
  "in" *> ss
  e <- parseExpr <* ss

  return $ Let d e

parseDefn :: Parser Defn
parseDefn =
  Val <$> ("val" *> ss *> atom <* ss <* char '=' <* ss)  <*> parseExpr <|>
  Rec <$> ("rec" *> ss *> atom <* ss <* char '=' <* ss)  <*> parseExpr

parseLam :: Parser Expr
parseLam = do
  ins <- char '\\' *> many1 (atom <* ss)
  "->" *> ss
  expr <- parseExpr

  return $ Lambda ins expr

parseApply :: Parser Expr
parseApply = do
  f <- "%{" *> ss *> parseExpr <* ss
  char ':' *> ss
  ins <- many' (parseExpr <* ss)
  char '}' *> ss

  return $ Apply f ins

parsePlusMinus :: Parser Expr
parsePlusMinus = parsePlusMinus' <|> parseTerm

parsePlusMinus' :: Parser Expr
parsePlusMinus' = do
  t <- parseTerm <* ss
  con <-
    char '+' *> pure Plus <|>
    char '-' *> pure Minus
  ss
  e <- parseExpr

  return $ con t e

parseTerm :: Parser Expr
parseTerm = parseTerm' <|> parseFactor

parseTerm' :: Parser Expr
parseTerm' = do
  f <- parseFactor <* ss
  "==" *> ss
  t <- parseTerm

  return $ Equals f t

parseFactor :: Parser Expr
parseFactor =
  char '(' *> parseExpr <* char ')' <|> parseExpr'

ss :: Parser ()
ss = skipSpace

atom :: Parser String
atom = T.unpack <$> takeWhile1 (\c -> c /= ' ' && c /= '"' && c /= '-' && c /= ':' && c /= '}')
