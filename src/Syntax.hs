module Syntax where

type Name = String

data Term = Variable Name
          | Lambda Name Term
          | Application Term Term
          deriving (Eq)

instance Show Term where
    show (Variable v)        = show v
    show (Lambda n t)        = "λ" ++ (show n) ++ ".[" ++ (show t) ++ "]"
    show (Application t1 t2) = "(" ++ (show t1) ++ " " ++ (show t2) ++ ")"


