module Reducer where

import Control.Monad.State

import Prelude hiding (null,filter,map,rem)
import qualified Data.List as List
import Data.Set hiding (fold)
import Data.Tree
import System.Console.Haskeline

import Syntax

--------------------------------------------------------------------------------
-- folding set operations on a lambda expression
-- used in vars and freeVars
---- insert and delete have same signature: (LambdaVar -> a -> a) 
---- union has signature: (a -> a -> a)
---- singleton has signature: (LambdaVar -> a)
---- returns Set a
fold :: (LambdaVar -> a -> a) -> (a -> a -> a) -> (LambdaVar -> a) -> Expression -> a
fold _ _ h (Variable v)     = h v
fold f g h (Abstraction v e) = f v (fold f g h e) 
fold f g h (Application e1 e2)  = g (fold f g h e1) (fold f g h e2)

--------------------------------------------------------------------------------
-- extracting all variales from a input expression
---- returns Set of lambda variables
vars :: Expression -> Set LambdaVar
vars = fold insert union singleton

--------------------------------------------------------------------------------
-- extracting free variales from a input expression
---- returns Set of lambda variables
freeVars :: Expression -> Set LambdaVar
freeVars = fold delete union singleton

--------------------------------------------------------------------------------
-- extracting bound variales from a input expression
---- returns Set of lambda variables
boundVars :: Expression -> Set LambdaVar
boundVars ex = difference (vars ex) (freeVars ex)

--------------------------------------------------------------------------------
-- returns a functions, which substitutes all free* occurences of x by n
---- if expression is a variable: sub if names match, else return same
---- if expression is an application: sub both applicants
---- if expression is an abstraction:
------ if vars are the same, leave unchanged
------ if x is NOT in free vars in the body, there is no x to be substituted
------ if x is in free vars in the body AND abstraction var is not in free vars of n, substitute in the body
------ else, we need a fresh var because abstraction variable y is in free vars of term to be substituted. fresh var is y'
sub :: LambdaVar -> Expression -> Expression -> Expression
sub x n (Variable y) | x == y = n 
                     | x /= y = Variable y  
sub x n (Application p q) = Application (sub x n p) (sub x n q)
sub x n (Abstraction y@(LambdaVar name num) p)
  | x == y                                               = Abstraction y p 
  | not $ member x (freeVars p)                          = Abstraction y p
  | member x (freeVars p) && not (member y (freeVars n)) = Abstraction y (sub x n p)
  | member x (freeVars p) && member y (freeVars n)       = 
     let new = LambdaVar name (num+1) 
     in Abstraction new $ sub x n $ sub y (Variable new) p

--------------------------------------------------------------------------------
-- ALPHA equivalence
--------------------------------------------------------------------------------
-- returns whether 2 lambda expressions are alpha equivalent 
---- variables derive Eq
---- applications are alpha equivalent if their corresponding parts are
---- we substitute bounding var of one abstraction into body of another and compare bodies
alphaEquiv :: Expression -> Expression -> Bool
alphaEquiv (Variable x) (Variable y)   = x == y
alphaEquiv (Application a b) (Application x y) = alphaEquiv a x && alphaEquiv b y
alphaEquiv (Abstraction x f) (Abstraction y g) =
  alphaEquiv f $ sub y (Variable x) g
alphaEquiv _ _ = False

--------------------------------------------------------------------------------
-- BETA reduction
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- finds all beta redexes of given lambda term
----- variable has no redexes
----- application of abstraction to expression is itself a redex. concatenate it with any inner redexes
----- redexes of (other type) application are just concatenated redexes of applicants
----- redexes of abstraction are redexes of its body
betaRedexes :: Expression -> [Expression] 
betaRedexes (Variable _)                = [] 
betaRedexes e@(Application (Abstraction _ e1) e2) = e : (betaRedexes e1 ++ betaRedexes e2)
betaRedexes (Application e1 e2)  = betaRedexes e1 ++ betaRedexes e2
betaRedexes (Abstraction _ f)    = betaRedexes f

--------------------------------------------------------------------------------
-- determines whether given lambda expression contains at least one beta redex
hasBetaRedex :: Expression -> Bool
hasBetaRedex = not . List.null . betaRedexes

--------------------------------------------------------------------------------
-- performs one step beta reduction and count steps
---- application of abstraction to an expression beta reduces by definition substituting expression for bounding var in the body of abstraction
---- (other type) application reduces its applicants (first left one to beta nf)
---- reducing abstraction is reducing its body
---- variable doesnt reduce
betaReduction :: Int -> Expression -> (Expression, Int)
betaReduction n (Application (Abstraction v e) a) = (sub v a e, n+1)
betaReduction n (Application e1 e2)          
  | hasBetaRedex e1  = do
      let e1b = betaReduction n e1
      (Application (fst e1b) e2, snd e1b)
  | otherwise  = do
      let e2b = betaReduction n e2 
      (Application e1 (fst e2b), snd e2b)
betaReduction n (Abstraction v e) = do
  let eb = betaReduction n e
  (Abstraction v (fst eb), snd eb)
betaReduction n (Variable v) = (Variable v, n)

--------------------------------------------------------------------------------
-- computes the beta normal form of a lambda term and count steps
---- do one step beta reduction if there are any redexes left
betaNF :: Int -> Expression -> (Expression, Int)
betaNF n ex
  | hasBetaRedex ex = do
      let exb = betaReduction n ex
      betaNF (snd exb) (fst exb)
  | otherwise       = (ex, n)



