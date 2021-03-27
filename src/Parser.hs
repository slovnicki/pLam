module Parser where

import Control.Monad.State
import Data.Char
import Debug.Trace
import Syntax
import Text.Parsec hiding (State)
import Text.Parsec.Language
import Text.Parsec.Token qualified as Token

-------------------------------------------------------------------------------------
languageDef =
  emptyDef
    { Token.commentLine = "--",
      Token.identStart = letter,
      Token.identLetter = alphaNum <|> char '_',
      Token.reservedNames =
        [ ":import",
          ":review",
          ":run",
          ":print",
          ":d",
          ":cbv"
        ],
      Token.reservedOpNames =
        [ "=",
          ".",
          "\\",
          "[",
          "]"
        ]
    }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer

reserved = Token.reserved lexer

reservedOp = Token.reservedOp lexer

parens = Token.parens lexer

comma = Token.comma lexer

-------------------------------------------------------------------------------------

type Parser = Parsec String ()

-------------------------------------------------------------------------------------
symbol :: Parser Char
symbol = oneOf ".`#~@$%^&*_+-=|;',/?[]<>(){} "

comment :: Parser String
comment = many $ symbol <|> letter <|> digit

filename :: Parser String
filename = many1 $ letter <|> symbol <|> digit

createChurch :: Int -> Expression -> Expression
createChurch 0 exp = Abstraction (LambdaVar 'f' 0) (Abstraction (LambdaVar 'x' 0) exp)
createChurch n exp = createChurch (n -1) (Application (Variable (LambdaVar 'f' 0)) exp)

-- HELP EXPRS --
true = Abstraction (LambdaVar 'x' 0) (Abstraction (LambdaVar 'y' 0) (Variable (LambdaVar 'x' 0)))

false = Abstraction (LambdaVar 'x' 0) (Abstraction (LambdaVar 'y' 0) (Variable (LambdaVar 'y' 0)))

pair = Abstraction (LambdaVar 'x' 0) (Abstraction (LambdaVar 'y' 0) (Abstraction (LambdaVar 'p' 0) (Application (Application (Variable (LambdaVar 'p' 0)) (Variable (LambdaVar 'x' 0))) (Variable (LambdaVar 'y' 0)))))

end = Abstraction (LambdaVar 'e' 0) true

whichBit :: Int -> Expression
whichBit b
  | b == 0 = false
  | b == 1 = true

----------------
createBinary' :: Int -> Expression
createBinary' 0 = end
createBinary' n = Application (Application pair (whichBit (mod n 2))) (createBinary' (quot n 2))

----------------
createBinary :: Int -> Expression
createBinary 0 = Application (Application pair false) end
createBinary n = createBinary' n

-- LIST --
empty = Abstraction (LambdaVar 'f' 0) (Abstraction (LambdaVar 'l' 0) (Variable (LambdaVar 'f' 0)))

createList :: [Expression] -> Expression
createList [] = empty
createList (x : xs) = Abstraction (LambdaVar 'f' 0) (Abstraction (LambdaVar 'l' 0) (Application (Application (Variable (LambdaVar 'l' 0)) x) (createList xs)))

-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
parseList :: Parser Expression
parseList = do
  reservedOp "["
  exprs <- parseExpression `sepBy` comma
  reservedOp "]"
  return $ createList exprs

parseNumeral :: Parser Expression
parseNumeral = do
  strNum <- many1 digit
  spaces
  let intNum = read strNum :: Int
  maybeB <- optionMaybe (char 'b')
  return $
    if maybeB == Just 'b'
      then createBinary intNum
      else createChurch intNum (Variable (LambdaVar 'x' 0))

parseVariable :: Parser Expression
parseVariable = do
  x <- identifier
  spaces
  return $
    if length x == 1 && isLower (head x)
      then Variable (LambdaVar (head x) 0)
      else EnvironmentVar x

parseAbstraction :: Parser Expression
parseAbstraction = do
  reservedOp "\\"
  xs <- endBy1 letter spaces
  reservedOp "."
  spaces
  curry xs <$> parseExpression
  where
    curry (x : xs) body = Abstraction (LambdaVar x 0) $ curry xs body
    curry [] body = body

parseApplication :: Parser Expression
parseApplication = do
  es <- sepBy1 parseSingleton spaces
  return $ foldl1 Application es

parseSingleton :: Parser Expression
parseSingleton =
  parseList
    <|> parseNumeral
    <|> parseVariable
    <|> parseAbstraction
    <|> parens parseApplication

parseExpression :: Parser Expression
parseExpression = do
  spaces
  expr <- parseApplication <|> parseSingleton
  spaces
  return expr

-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
parseDefine :: Parser Command
parseDefine = do
  var <- identifier
  spaces
  reservedOp "="
  spaces
  Define var <$> parseExpression

-- Evaluate with options
parseDetailed :: Parser EvaluateOption
parseDetailed = do
  reserved ":d"
  return Detailed

parseCallByValue :: Parser EvaluateOption
parseCallByValue = do
  reserved ":cbv"
  return CallByValue

parseEvaluate :: Parser Command
parseEvaluate = do
  det <- option None parseDetailed
  spaces
  cbv <- option None parseCallByValue
  spaces
  Evaluate det cbv <$> parseExpression

-----------------------------------

parseImport :: Parser Command
parseImport = do
  reserved ":import"
  spaces
  Import <$> filename

parseExport :: Parser Command
parseExport = do
  reserved ":export"
  spaces
  Export <$> filename

parseReview :: Parser Command
parseReview = do
  reserved ":review"
  spaces
  Review <$> identifier

parseComment :: Parser Command
parseComment = do
  comm <- string "--"
  Comment <$> comment

parseEmptyLine :: Parser Command
parseEmptyLine = do
  emp <- string ""
  return $ Comment " "

parseRun :: Parser Command
parseRun = do
  reserved ":run"
  spaces
  Run <$> filename

parsePrint :: Parser Command
parsePrint = do
  reserved ":print"
  spaces
  Print <$> comment

parseLine :: Parser Command
parseLine =
  try parseDefine
    <|> parseImport
    <|> parseExport
    <|> parseReview
    <|> parseRun
    <|> parsePrint
    <|> parseComment
    <|> parseEvaluate
    <|> parseEmptyLine

-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
readLine :: String -> Failable Command
readLine input = case parse parseLine "parser" input of
  Left err -> Left $ SyntaxError err
  Right l -> Right l
