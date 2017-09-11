module Reductions where

import Control.Monad.State
import Debug.Trace

import Prelude hiding (null,filter,map,length,rem)
import qualified Data.List as List
import Data.Set hiding (fold)
import Data.Tree

import Syntax


-- a catamorphism for lambda terms
--      lambda                  app              lvar
fold :: (LambdaVar -> a -> a) -> (a -> a -> a) -> (LambdaVar -> a) -> Expression -> a
fold _ _ h (Variable v)     = h v
fold f g h (Abstraction v e) = f v (fold f g h e) 
fold f g h (Application e1 e2)  = g (fold f g h e1) (fold f g h e2)

vars :: Expression -> Set LambdaVar
vars = fold insert union singleton

freeVars :: Expression -> Set LambdaVar
freeVars = fold delete union singleton

boundVars :: Expression -> Set LambdaVar
boundVars ex = difference (vars ex) (freeVars ex)

freshTermVar :: Expression -> LambdaVar
freshTermVar = head . toList . freshVars 1 . vars

-- returns n distinct fresh variables which do not occour in given set. Note
-- that this function works on an arbitrary variable set.
freshVars :: Int -> Set LambdaVar -> Set LambdaVar
freshVars n = fromList . getFreshN' n . toList where

  getFreshN' :: Int -> [LambdaVar] -> [LambdaVar]
  getFreshN' n []             = take n $ zipWith LambdaVar ['x'..] [0,1..]
  getFreshN' n (v@(LambdaVar c _):vs) =
    let m = maximum $ 
            List.map index $ 
            List.filter ((==c) . name) 
            (v:vs)
    in take n $ zipWith LambdaVar [c,c..] [1+m,2+m..]

length :: Expression -> Int
length = fold (\_ l -> 1+l) (+) (const 1)

-- |all subterms of given lambda term, see [1] 1A3
subterms :: Expression -> [Expression]
subterms e@(Variable _)     = [e]
subterms e@(Abstraction _ t) = e : subterms t
subterms e@(Application e1 e2)  = e : (subterms e1 ++ subterms e2)

-- |a lambda term is closed iff it has no free variables
closed :: Expression -> Bool
closed = null . freeVars

-- |returns a functions, which substitutes all free occourences of @x@ by @n@ 
sub :: LambdaVar -> Expression -> Expression -> Expression
sub x n (Variable y) | x == y = n 
                 | x /= y = Variable y  
sub x n (Application p q) = Application (sub x n p) (sub x n q)
sub x n (Abstraction y p)
  | x == y                                               = Abstraction x p 
  | not $ member x (freeVars p)                          = Abstraction y p
  | member x (freeVars p) && not (member y (freeVars n)) = Abstraction y (sub x n p)
  | member x (freeVars p) && member y (freeVars n)       = 
     let z = freshTermVar $ Application n p 
     in Abstraction z $ sub x n $ sub y (Variable z) p

-- BETA reduction
-- |returns all beta redexes of given lambda term
betaRedexes :: Expression -> [Expression] 
betaRedexes (Variable _)                = [] 
betaRedexes e@(Application (Abstraction _ e1) e2) = e : (betaRedexes e1 ++ betaRedexes e2)
betaRedexes (Application e1 e2)              = betaRedexes e1 ++ betaRedexes e2
betaRedexes (Abstraction _ f)             = betaRedexes f

-- |determines whether given lambda term contains at least one beta redex
hasBetaRedex :: Expression -> Bool
hasBetaRedex = not . List.null . betaRedexes

-- |determines whether given lambda term is in beta normal form, i.e. contains
-- no beta redex
inBetaNF :: Expression -> Bool
inBetaNF = not . hasBetaRedex

-- |computes the beta normal form of a lambda term
betaNF :: Expression -> Expression
betaNF t
  | hasBetaRedex t = betaNF $ betaReduction t
  | otherwise      = t

-- |one step beta reduction
betaReduction :: Expression -> Expression
betaReduction (Application (Abstraction v e) a) = sub v a e
betaReduction (Application e1 e2)          
  | e1 == betaReduction e1 = Application e1 (betaReduction e2)
  | otherwise               = Application (betaReduction e1) e2
betaReduction (Abstraction v e)         = Abstraction v $ betaReduction e
betaReduction (Variable v)             = Variable v

-- * visualization of reductions

-- ** a sequential visualization of left-outermost beta reduction

betaNFSteps :: Expression -> [Expression]
betaNFSteps l = case betaRedexes l of
  []     -> [l]
  (x:_)  -> x : betaNFSteps (betaReduction l)

visualizeBeta :: Expression -> IO ()
visualizeBeta l = 
  let betas = betaNFSteps l in mapM_ (putStrLn . (\s -> " ==> " ++ s) .  show) betas

-- ** tree based visualization of all possible beta reductions

rList :: (Expression -> Expression) -> Expression -> Set Expression
rList f (Variable v)  = singleton $ f $ Variable v
rList f p@(Application m n) = insert (f $ betaReduction p) $ 
  rList (\x -> f $ Application x n) m  `union` rList (f . Application m) n 
rList f (Abstraction v e) = rList (f . Abstraction v) e

-- |builds the tree of all possible reductions
rTree :: Expression -> Tree Expression
rTree x = Node x (List.map rTree . toList . delete x $ rList id x)

-- |draws the tree with all possible reductions
drawPossibleReductions :: Expression -> IO ()
drawPossibleReductions = putStrLn . drawTree . fmap show . rTree 

-- recreate expression from environment
{-replaceAllVars :: Expression -> Program (Failable Expression)
replaceAllVars var@(Variable lv@(LambdaVar v i)) = state $ \e -> (reference lv e, e) where
    reference lv e = case lookup lv e of
        Nothing -> Right var
        Just x  -> Right x
replaceAllVars (Abstraction v e) = return $ Right $ Abstraction v (Right $ replaceAllVars v)
replaceAllVars (Application m n) = return $ Right $ Appplication (Right $ replaceAllVars m) (Right $ replaceAllVars n)
-}
-----------------------

alphaEquiv :: Expression -> Expression -> Bool
alphaEquiv (Variable x) (Variable y)   = x == y
alphaEquiv (Application a b) (Application x y) = alphaEquiv a x && alphaEquiv b y
alphaEquiv (Abstraction x f) (Abstraction y g) =
  alphaEquiv f $ sub y (Variable x) g
alphaEquiv _ _ = False





