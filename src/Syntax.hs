module Syntax where

type Name = String

data Term = Variable Name
          | Lambda Name Term
          | Application Term Term
          deriving (Eq, Show)


