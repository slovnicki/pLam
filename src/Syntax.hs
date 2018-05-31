module Syntax where

import Control.Monad.State
import Text.Parsec hiding (State)


-------------------------------------------------------------------------------------
data LambdaVar = LambdaVar { name :: Char
                           , index :: Int
                           } deriving (Ord,Eq)

showVarHelper :: Int -> String
showVarHelper 0 = ""
showVarHelper n = "'" ++ showVarHelper (n-1)

instance Show LambdaVar where
  show (LambdaVar c 0) = [c]
  show (LambdaVar c i) = [c] ++ showVarHelper i
-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
data Expression = Variable LambdaVar
                | Abstraction LambdaVar Expression
                | Application Expression Expression
                | EnvironmentVar String
                deriving (Ord,Eq)

instance Show Expression where
    show (Variable v)        = show v
    show (Abstraction n t)   = "\x1b[32m(λ\x1b[0m" ++ show n ++ "\x1b[32m.\x1b[0m" ++ show t ++ "\x1b[32m)\x1b[0m"
    show (Application t1 t2) = "\x1b[33m[\x1b[0m" ++ show t1 ++ " " ++ show t2 ++ "\x1b[33m]\x1b[0m"
    show (EnvironmentVar ev) = ev
-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
data Command = Define String Expression
             | Show Expression
             | ShowDetailed Expression
             | Import String
             | Review String
             | Comment String
             | Run String
             | Print String
             deriving (Eq, Show)
-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
type Environment = [(String, Expression)]

type Program = State Environment

data Error = SyntaxError ParseError
           | UndeclaredVariable String
           | FatalError String

instance Show Error where
    show (SyntaxError se)        = show se
    show (UndeclaredVariable uv) = " ERROR: undeclared variable " ++ show uv ++ "\n- type \":review all\" to see all environment variables you can use\n- type \"" ++ uv ++ " = <lambda expression>\" to add this variables to environment"
    show (FatalError fe)         = show fe

type Failable = Either Error
-------------------------------------------------------------------------------------


