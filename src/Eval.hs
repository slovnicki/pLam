module Eval where

import Control.Monad.State
import Debug.Trace

{-import Prelude hiding (null,filter,map,length,rem)
import qualified Data.List as List
import Data.Set hiding (fold)
import Data.Tree-}

import Syntax

evaluateVariable :: Variable -> Program (Failable Expression)
evaluateVariable a = state $ \e -> (reference a e, e) where
    reference a e = case lookup a e of
        Nothing -> Left $ UndeclaredVariable a
        Just x  -> Right x


evalLambda :: Variable -> Expression -> Program (Failable Expression)
evalLambda x y = return $ Right $ Abstraction x y


betaReduce :: Variable -> Expression -> Expression -> Expression
betaReduce b b' (Variable a)
    | a == b    = b'
    | otherwise = Variable a
betaReduce b b' (Abstraction x y)
    | b == x    = Abstraction x y
    | otherwise = Abstraction x $ betaReduce b b' y
betaReduce b b' (Application f x) = Application (betaReduce b b' f) (betaReduce b b' x)

applyLambda :: Expression -> Expression -> Program (Failable Expression)
applyLambda (Abstraction x y) x' = evalE $ betaReduce x x' y

evalApply :: Expression -> Expression -> Program (Failable Expression)
evalApply f x = do
    f' <- evalE f
    case f' of
        Left e    -> return $ Left e
        Right f'' -> applyLambda f'' x 


evalDefine :: Variable -> Expression -> Program (Failable Expression)
evalDefine x y = do
    y' <- evalE y
    case y' of
        Left e -> return $ Left e
        Right f -> do
            modify ((x, f):)
            return $ Right f

evalE :: Expression -> Program (Failable Expression)
evalE (Variable v) = evaluateVariable v
evalE (Abstraction v e) = evalLambda v e
evalE (Application m n) = evalApply m n


