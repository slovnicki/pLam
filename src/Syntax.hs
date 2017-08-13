module Syntax where

import Control.Monad.State
import Text.Parsec hiding (State)

type Variable = String

data Expression = Variable Variable
                | Abstraction Variable Expression
                | Application Expression Expression
                deriving (Eq)

instance Show Expression where
    show (Variable v)        = v
    show (Abstraction n t)   = "λ" ++ n ++ "." ++ show t
    show (Application t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

data Command = Assign Variable Expression
             deriving (Eq, Show)

data LCILine = Command Command
             | Expression Expression
             deriving (Eq, Show)

type Environment = [(Variable, Expression)]

type Program = State Environment

data Error = SyntaxError ParseError
           | UndeclaredVariable Variable
           | FatalError String
           deriving (Show)

type Failable = Either Error


