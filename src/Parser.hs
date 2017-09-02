module Parser where

import Control.Monad.State
import Text.Parsec hiding (State)
import Debug.Trace

import Syntax


type Parser = Parsec String ()

symbol :: Parser Char
symbol = oneOf "`~!@#$%^&*-_+|;:',/?[]<>"

fsymbol :: Parser Char
fsymbol = oneOf "."

identifier :: Parser String
identifier = many1 $ letter <|> symbol <|> digit

filename :: Parser String
filename = many1 $ letter <|> fsymbol <|> digit

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

----------------------------------------------------------------
parseDefine :: Parser Command
parseDefine = do
    comm <- string "define"
    spaces
    var <- identifier
    spaces
    char '='
    spaces
    y <- parseExpression
    return $ Assign var y

parseExecute :: Parser Command
parseExecute = do
    comm <- string "execute"
    spaces
    ex <- parseExpression
    return $ Execute ex

parseImport :: Parser Command
parseImport = do
    comm <- string "import"
    spaces
    f <- filename
    return $ Import f

parseReview :: Parser Command
parseReview = do
    comm <- string "review"
    spaces
    f <- identifier
    return $ Review f

---------------------------------------------------------------------
parseLine :: Parser Command
parseLine = parseDefine
         <|> parseExecute
         <|> parseImport
         <|> parseReview

readLine :: String -> Failable Command
readLine input = case parse parseLine "parser" input of
    Left err -> Left $ SyntaxError err
    Right l -> Right l 
    
readExpr :: String -> Failable Command
readExpr input = readLine input

