module Parser where

import Control.Monad.State
import Text.Parsec hiding (State)

import Syntax


type Parser = Parsec String ()

symbol :: Parser Char
symbol = oneOf "`~!@#$%^&*-_+|;:',/?[]<>"

identifier :: Parser String
identifier = many1 $ letter <|> symbol <|> digit

parseVariable :: Parser Expression
parseVariable = liftM Variable $ identifier

parseAbstraction :: Parser Expression
parseAbstraction = do
  char '\\'
  xs <- identifier `endBy1` spaces
  char '.'
  spaces
  body <- parseApplication
  return $ curry xs body where
        curry (x:xs) body = Abstraction x $ curry xs body
        curry [] body     = body

parseApplication :: Parser Expression
parseApplication = do
  es <- parseExpression `sepBy1` spaces
  return $ foldl1 Application es

parseParens :: Parser Expression
parseParens = between (char '(') (char ')') parseApplication

parseExpression :: Parser Expression
parseExpression = parseVariable 
               <|> parseAbstraction
               <|> parseParens

parseAssign :: Parser Command
parseAssign = do
    f <- identifier
    spaces
    xs <- identifier `endBy` spaces
    char '='
    spaces
    y <- parseAbstraction
    return $ Assign f $ curry xs y where
        curry (x:xs) y = Abstraction x $ curry xs y
        curry [] y     = y


{-
readCommand :: String -> Failable Command
readCommand input = case parse parseAssign "<stdin>" input of
  Left err -> Left $ SyntaxError err
  Right ex -> Right ex
-}

readLambda :: String -> Failable Expression
readLambda input = case parse parseExpression "<stdin>" input of
  Left err -> Left $ SyntaxError err
  Right ex -> Right ex

readExpr :: String -> Failable Expression
readExpr input = readLambda input

