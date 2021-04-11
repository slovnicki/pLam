module Syntax where

import Control.Monad.State
import Text.Parsec hiding (State)

-------------------------------------------------------------------------------------
data LambdaVar = LambdaVar
  { name :: Char,
    index :: Int
  }
  deriving (Ord, Eq)

showVarHelper :: Int -> String
showVarHelper 0 = ""
showVarHelper n = "'" <> showVarHelper (n - 1)

instance Show LambdaVar where
  show (LambdaVar c 0) = [c]
  show (LambdaVar c i) = c : showVarHelper i

-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
data Expression
  = Variable LambdaVar
  | Abstraction LambdaVar Expression
  | Application Expression Expression
  | EnvironmentVar String
  deriving (Ord, Eq)

uncurryShow :: Expression -> String
uncurryShow (Abstraction v1 (Abstraction v2 e)) = show v1 <> show v2 <> uncurryShow e
uncurryShow (Abstraction v e) = show v <> "." <> show e
uncurryShow (Variable v) = ". " <> show v
uncurryShow (Application e1 e2) = ". " <> show e1 <> " " <> show e2

instance Show Expression where
  show (Variable v) = show v
  show abs@(Abstraction _ _) = "(λ" <> uncurryShow abs <> ")"
  show (Application t1 t2) = "(" <> show t1 <> " " <> show t2 <> ")"
  show (EnvironmentVar ev) = ev

-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
data EvaluateOption
  = Detailed
  | CallByValue
  | None
  deriving (Eq, Show)

data Command
  = Define String Expression
  | Evaluate EvaluateOption EvaluateOption Expression
  | Import String
  | Export String
  | Review String
  | Comment String
  | Run String
  | Print String
  deriving (Eq, Show)

-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
type Environment = [(String, Expression)]

type Program = State Environment

data Error
  = SyntaxError ParseError
  | UndeclaredVariable String
  | FatalError String

instance Show Error where
  show (SyntaxError se) = show se
  show (UndeclaredVariable uv) =
    " ERROR: undeclared variable "
      <> show uv
      <> "\n- type \":review all\" to see all environment variables you can use\n- type \""
      <> uv
      <> " = <lambda expression>\" to add this variables to environment"
  show (FatalError fe) = show fe

type Failable = Either Error

-------------------------------------------------------------------------------------
