module Eval where

import Control.Monad.State
import Debug.Trace

import Syntax
import Reductions

evaluateVariable :: LambdaVar -> Program (Failable Expression)
evaluateVariable a = state $ \e -> (reference a e, e) where
    reference a e = case lookup a e of
        Nothing -> Left $ UndeclaredVariable a
        Just x  -> Right x


evalLambda :: LambdaVar -> Expression -> Program (Failable Expression)
evalLambda x y = return $ Right $ Abstraction x y

--applyLambda :: Expression -> Program (Failable Expression)
--applyLambda app = evalE app

evalApply :: Expression -> Expression -> Program (Failable Expression)
evalApply f x = do
    f' <- evalE f
    case f' of
        Left e    -> return $ Left e
        Right f'' -> do
            x' <- evalE x
            case x' of
                Left e -> return $ Left e
                Right x'' -> return $ Right $ Application f'' x'' 
{-
evalApply :: Expression -> Expression -> Program (Failable Expression)
evalApply f x = do
    f' <- trace("-- lhs: " ++ show f) (evalE f)
    case f' of
        Left err    -> return $ Left err
        Right f'' -> do
            case f'' of
                Variable var -> evaluateVariable var
                Abstraction v e -> trace ("-- before apply: " ++ show f'' ++ " to " ++ show x) (applyLambda (Application f'' x))
                app@(Application m n) -> evalE app
-}                

evalE :: Expression -> Program (Failable Expression)
evalE (Variable v) = evaluateVariable v
evalE (Abstraction v e) = evalLambda v e--return $ Right $ Abstraction v e
evalE (Application m n) = evalApply m n

evalDefine :: LambdaVar -> Expression -> Program (Failable Expression)
evalDefine x y = do
    y' <- evalE y
    case y' of
        Left e -> return $ Left e
        Right f -> do
            modify ((x, f):)
            return $ Right f


