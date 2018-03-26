module Parser where

import Control.Monad.State
import Text.Parsec hiding (State)
import Debug.Trace
import Data.Char

import qualified Text.Parsec.Token as Token
import Text.Parsec.Language

import Syntax

-------------------------------------------------------------------------------------
languageDef =
    emptyDef { Token.commentLine     = "--"
             , Token.identStart      = letter
             , Token.identLetter     = alphaNum
             , Token.reservedNames   = [ ":import"
                                       , ":review"
                                       , ":run"
                                       , ":print"
                                       , ":d"
                                       , ":b"
                                       ]
             , Token.reservedOpNames = [ "="
                                       , "." 
                                       , "\\"
                                       ]
             }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer 
reservedOp = Token.reservedOp lexer 
parens     = Token.parens     lexer
-------------------------------------------------------------------------------------

type Parser = Parsec String ()

-------------------------------------------------------------------------------------
symbol :: Parser Char
symbol = oneOf ".`#~@$%^&*_+-=|;',/?[]<>(){} "

comment :: Parser String
comment = many $ symbol <|> letter <|> digit

filename :: Parser String
filename = many1 $ letter <|> symbol <|> digit

fromNumber :: Int -> Expression -> Expression
fromNumber 0 exp = Abstraction (LambdaVar 'f' 0) (Abstraction (LambdaVar 'x' 0) exp)
fromNumber n exp = fromNumber (n-1) (Application (Variable (LambdaVar 'f' 0)) exp)

-- HELP EXPRS --
true = Abstraction (LambdaVar 'x' 0) (Abstraction (LambdaVar 'y' 0) (Variable (LambdaVar 'x' 0)))
false = Abstraction (LambdaVar 'x' 0) (Abstraction (LambdaVar 'y' 0) (Variable (LambdaVar 'y' 0)))
pair = Abstraction (LambdaVar 'x' 0) (Abstraction (LambdaVar 'y' 0) (Abstraction (LambdaVar 'p' 0) (Application (Application (Variable (LambdaVar 'p' 0)) (Variable (LambdaVar 'x' 0))) (Variable (LambdaVar 'y' 0)))))
end = Abstraction (LambdaVar 'e' 0) true
whichBit :: Int -> Expression
whichBit b
    | b == 0    = false
    | otherwise = true
----------------
fromBinary :: Int -> Expression
fromBinary 0 = end
fromBinary n = Application (Application (pair) (whichBit (mod n 2))) (fromBinary (quot n 2))  
-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
parseChurch :: Parser Expression
parseChurch = do
    strNum <- many1 digit
    let intNum = read strNum :: Int
    return (fromNumber intNum (Variable (LambdaVar 'x' 0)))

parseBinary :: Parser Expression
parseBinary = do
    reservedOp ":b"
    strNum <- many1 digit
    let intNum = read strNum :: Int
    return (fromBinary intNum)

parseVariable :: Parser Expression
parseVariable = do
    x <- identifier
    spaces
    case (length x) of
        1 -> return (Variable (LambdaVar (head x) 0))
        otherwise -> return (EnvironmentVar x) 

parseAbstraction :: Parser Expression
parseAbstraction = do
  reservedOp "\\"
  xs <- endBy1 letter spaces
  reservedOp "."
  spaces
  body <- parseApplication
  return $ curry xs body where
        curry (x:xs) body = Abstraction (LambdaVar x 0) $ curry xs body
        curry [] body     = body

parseApplication :: Parser Expression
parseApplication = do
  es <- sepBy1 parseSingleton spaces
  return $ foldl1 Application es

parseExpression :: Parser Expression
parseExpression =  parens parseApplication
               <|> parseApplication
               <|> parseSingleton

parseSingleton :: Parser Expression
parseSingleton =  parseChurch
              <|> parseBinary
              <|> parseVariable
              <|> parseAbstraction
              <|> parens parseApplication
-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
parseDefine :: Parser Command
parseDefine = do
    var <- identifier
    spaces
    reservedOp "="
    spaces
    y <- parseExpression
    return $ Define var y

parseShow :: Parser Command
parseShow = do
    ex <- parseExpression
    return $ Show ex

parseShowDetailed :: Parser Command
parseShowDetailed = do
    reserved ":d"
    spaces
    ex <- parseExpression
    return $ ShowDetailed ex

parseImport :: Parser Command
parseImport = do
    reserved ":import"
    spaces
    f <- filename
    return $ Import f

parseReview :: Parser Command
parseReview = do
    reserved ":review"
    spaces
    f <- identifier
    return $ Review f

parseComment :: Parser Command
parseComment = do
    comm <- string "--"
    c <- comment
    return $ Comment c

parseEmptyLine :: Parser Command
parseEmptyLine = do
    emp <- string ""
    return $ Comment " "

parseRun :: Parser Command
parseRun = do
    reserved ":run"
    spaces
    f <- filename
    return $ Run f

parsePrint :: Parser Command
parsePrint = do
    reserved ":print"
    spaces
    str <- comment
    return $ Print str 
    
parseLine :: Parser Command
parseLine =  try parseDefine
         <|> parseShowDetailed
         <|> parseImport
         <|> parseReview
         <|> parseRun
         <|> parsePrint
         <|> parseComment
         <|> parseShow
         <|> parseEmptyLine
-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
readLine :: String -> Failable Command
readLine input = case parse parseLine "parser" input of
    Left err -> Left $ SyntaxError err
    Right l -> Right l

