module Evaluator where

import Control.Monad.State
import Data.Char
import Debug.Trace
import Syntax

-------------------------------------------------------------------------------------
evalVar :: String -> Program (Failable Expression)
evalVar a = state $ \e ->
  ( case lookup a e of
      Nothing -> Left $ UndeclaredVariable a
      Just x -> Right x,
    e
  )

evalAbs :: LambdaVar -> Expression -> Program (Failable Expression)
evalAbs x@(LambdaVar n i) y = evalExp y >>= (pure . fmap (Abstraction x))

evalApp :: Expression -> Expression -> Program (Failable Expression)
evalApp f x =
  evalExp f
    >>= ( \case
            Left e -> pure $ Left e
            Right f'' -> fmap (Application f'') <$> evalExp x
        )

evalExp :: Expression -> Program (Failable Expression)
evalExp x@(Variable (LambdaVar n i)) = pure $ Right x --evalVar ([n] <> showVarHelper i)
evalExp (Abstraction v e) = evalAbs v e
evalExp (Application m n) = evalApp m n
evalExp (EnvironmentVar ev) = evalVar ev

-------------------------------------------------------------------------------------

evalDefine :: String -> Expression -> Program (Failable Expression)
evalDefine x y =
  evalExp y
    >>= ( \case
            Left e -> pure $ Left e
            Right f -> modify ((x, f) :) >> pure (Right f)
        )
