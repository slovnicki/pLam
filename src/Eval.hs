module Eval where

import Control.Monad.State
import Debug.Trace

import Syntax
import Reductions

evaluateEnvironmentVar :: String -> Program (Failable Expression)
evaluateEnvironmentVar a = state $ \e -> (reference a e, e) where
    reference a e = case lookup a e of
        Nothing -> Left $ UndeclaredVariable a
        Just x  -> Right x

evalLambda :: LambdaVar -> Expression -> Program (Failable Expression)
evalLambda x y = do
    y' <- evalE y
    case y' of
        Left err  -> return $ Left err
        Right y'' -> return $ Right $ Abstraction x y''

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

evalE :: Expression -> Program (Failable Expression)
evalE (Variable v) = return $ Right $ Variable v
evalE (Abstraction v e) = evalLambda v e
evalE (Application m n) = evalApply m n
evalE (EnvironmentVar ev) = evaluateEnvironmentVar ev

evalDefine :: String -> Expression -> Program (Failable Expression)
evalDefine x y = do
    y' <- evalE y
    case y' of
        Left e -> return $ Left e
        Right f -> do
            modify ((x, f):)
            return $ Right f


