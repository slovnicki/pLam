module Syntax where

import Control.Monad.State
import Text.Parsec hiding (State)

data LambdaVar = LambdaVar { name :: Char
                           , index :: Int
                           } deriving (Ord,Eq)

showHelper :: Int -> String
showHelper 0 = ""
showHelper n = "'" ++ showHelper (n-1)
instance Show LambdaVar where
  show (LambdaVar c 0) = [c]
  show (LambdaVar c i) = [c] ++ showHelper i

data Expression = Variable LambdaVar
                | Abstraction LambdaVar Expression
                | Application Expression Expression
                | EnvironmentVar String
                deriving (Ord,Eq)

instance Show Expression where
    show (Variable v)        = show v
    show (Abstraction n t)   = "λ" ++ show n ++ "." ++ show t
    show (Application t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (EnvironmentVar ev)         = ev

data Command = Define String Expression
             | Execute String Expression
             | Import String
             | Review String
             | Comment String
             deriving (Eq, Show)

data LCILine = Command Command
             | Expression Expression
             deriving (Eq, Show)

type Environment = [(String, Expression)]

type Program = State Environment

data Error = SyntaxError ParseError
           | UndeclaredVariable String
           | FatalError String

instance Show Error where
    show (SyntaxError se)        = show se
    show (UndeclaredVariable uv) = " ERROR: undeclared variable " ++ show uv ++ "\n- type \"review all\" to see all environment variables you can use\n- type \"define <variable name> = <lambda expression>\" to add new variables to environment"
    show (FatalError fe)         = show fe

type Failable = Either Error


