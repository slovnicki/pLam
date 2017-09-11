module Parser where

import Control.Monad.State
import Text.Parsec hiding (State)
import Debug.Trace
import Data.Char

import Syntax


type Parser = Parsec String ()

symbol :: Parser Char
symbol = oneOf "`~!@$%^&*-_+|;:',/?[]<>"

fsymbol :: Parser Char
fsymbol = oneOf "."

num_symbol :: Parser Char
num_symbol = oneOf "#"

cspace :: Parser Char
cspace = oneOf " "

identifier :: Parser Char
identifier = letter

comment :: Parser String
comment = many $ letter <|> symbol <|> fsymbol <|> num_symbol <|> digit <|> cspace

filename :: Parser String
filename = many1 $ letter <|> fsymbol <|> digit

-------------------------------------------------------------------------------------
parseVariable :: Parser Expression
parseVariable = do
    x <- letter
    return (Variable (LambdaVar x 0))  

parseAbstraction :: Parser Expression
parseAbstraction = do
  char '\\'
  xs <- identifier `endBy1` spaces
  char '.'
  spaces
  body <- parseApplication
  return $ curry xs body where
        curry (x:xs) body = Abstraction (LambdaVar x 0) $ curry xs body
        curry [] body     = body

parseApplication :: Parser Expression
parseApplication = do
  es <- parseExpression `sepBy1` spaces
  return $ foldl1 Application es

parseParens :: Parser Expression
parseParens = between (char '(') (char ')') parseApplication

--------------------------------
fromNumber :: Int -> Expression -> Expression
fromNumber 0 exp = Abstraction (LambdaVar 'f' 0) (Abstraction (LambdaVar 'x' 0) exp)
fromNumber n exp = fromNumber (n-1) (Application (Variable (LambdaVar 'f' 0)) exp)

parseChurch :: Parser Expression
parseChurch = do
    hash <- num_symbol
    strNum <- many1 digit
    let intNum = read strNum :: Int
    return (fromNumber intNum (Variable (LambdaVar 'x' 0)))
--------------------------------

parseExpression :: Parser Expression
parseExpression = parseVariable 
               <|> parseAbstraction
               <|> parseParens
               <|> parseChurch

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
    return $ Define (LambdaVar var 0) y

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
    f <- many1 letter
    return $ Review f

parseComment :: Parser Command
parseComment = do
    comm <- string "--"
    c <- comment
    return $ Comment c
    

---------------------------------------------------------------------
parseLine :: Parser Command
parseLine = parseDefine
         <|> parseExecute
         <|> parseImport
         <|> parseReview
         <|> parseComment

readLine :: String -> Failable Command
readLine input = case parse parseLine "parser" input of
    Left err -> Left $ SyntaxError err
    Right l -> Right l 
    
readExpr :: String -> Failable Command
readExpr input = readLine input

