module Parser (parseExpr) where

import Data.Char
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Language (emptyDef)

import qualified Text.ParserCombinators.Parsec.Token as Token

import Syntax


lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef

variable :: Parser Term
variable = do
  x <- Token.identifier lexer
  return (Variable x)

lambda :: Parser Term
lambda = do
  (Token.reservedOp lexer) "\\"
  args <- many1 (Token.identifier lexer)
  (Token.reservedOp lexer) "."
  body <- expr
  return $ foldr Lambda body args

term :: Parser Term
term =  (Token.parens lexer) expr
    <|> variable
    <|> lambda

expr :: Parser Term
expr = do
  es <- many1 term
  return (foldl1 Application es)

parseExpr :: String -> Either ParseError Term
parseExpr input = parse (expr) "<stdin>" input


