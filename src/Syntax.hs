module Syntax where

import Control.Monad.State
import Text.Parsec hiding (State)

data LambdaVar = LambdaVar { name :: Char
                           , index :: Int
                           } deriving (Ord,Eq)
instance Show LambdaVar where
  show (LambdaVar c 0) = id (show c)
  show (LambdaVar c i) = id (show c ++ show i)

data Expression = Variable LambdaVar
                | Abstraction LambdaVar Expression
                | Application Expression Expression
                deriving (Ord,Eq)

instance Show Expression where
    show (Variable v)        = show v
    show (Abstraction n t)   = "λ" ++ show n ++ "." ++ show t
    show (Application t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

data Command = Define LambdaVar Expression
             | Execute String Expression
             | Import String
             | Review String
             | Comment String
             deriving (Eq, Show)

data LCILine = Command Command
             | Expression Expression
             deriving (Eq, Show)

type Environment = [(LambdaVar, Expression)]

type Program = State Environment

data Error = SyntaxError ParseError
           | UndeclaredVariable LambdaVar
           | FatalError String

instance Show Error where
    show (SyntaxError se)        = show se
    show (UndeclaredVariable uv) = " ERROR: undeclared variable " ++ show uv ++ "\n- type \"review all\" to see all environment variables you can use\n- type \"define <variable name> = <lambda expression>\" to add new variables to environment"
    show (FatalError fe)         = show fe

type Failable = Either Error


