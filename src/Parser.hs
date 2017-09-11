module Parser where

import Control.Monad.State
import Text.Parsec hiding (State)
import Debug.Trace
import Data.Char

import Syntax


type Parser = Parsec String ()

symbol :: Parser Char
symbol = oneOf "`#~@$%^&*-_+|;:',/?[]<>"

fsymbol :: Parser Char
fsymbol = oneOf "."

cspace :: Parser Char
cspace = oneOf " "

envIdentifier :: Parser String
envIdentifier = many1 $ letter <|> symbol <|> digit

comment :: Parser String
comment = many $ letter <|> symbol <|> fsymbol <|> digit <|> cspace

filename :: Parser String
filename = many1 $ letter <|> fsymbol <|> digit

-------------------------------------------------------------------------------------
parseVariable :: Parser Expression
parseVariable = do
    x <- letter
    return (Variable (LambdaVar x 0))  

parseEnvironmentVar :: Parser Expression
parseEnvironmentVar = do
    char '!'
    ev <- envIdentifier
    return (EnvironmentVar ev)

parseAbstraction :: Parser Expression
parseAbstraction = do
  char '\\'
  xs <- letter `endBy1` spaces
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
    strNum <- many1 digit
    let intNum = read strNum :: Int
    return (fromNumber intNum (Variable (LambdaVar 'x' 0)))
--------------------------------

parseExpression :: Parser Expression
parseExpression = parseVariable 
               <|> parseAbstraction
               <|> parseParens
               <|> parseChurch
               <|> parseEnvironmentVar

----------------------------------------------------------------
parseDefine :: Parser Command
parseDefine = do
    comm <- string "define"
    spaces
    char '!'
    spaces
    var <- envIdentifier
    spaces
    char '='
    spaces
    y <- parseExpression
    return $ Define var y

parseExecute :: Parser Command
parseExecute = do
    comm <- string "execute"
    spaces
    op <- many1 letter
    spaces
    ex <- parseExpression
    return $ Execute op ex

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
    f <- envIdentifier
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

