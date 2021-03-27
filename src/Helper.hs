module Helper where

import Control.Monad.State
import Debug.Trace
import Evaluator
import Parser
import Reducer
import Syntax
import System.Console.Haskeline
import System.IO (Handle, hFlush, hPutStrLn, stdout)

-------------------------------------------------------------------------------------
showGlobal :: (String, Expression) -> InputT IO ()
showGlobal (n, e) = outputStrLn ("--- " ++ show n ++ " = " ++ show e)

printGlobal :: (String, Expression) -> IO ()
printGlobal (n, e) = putStrLn ("--- " ++ show n ++ " = " ++ show e)

removeLambda :: String -> String
removeLambda =
  map
    ( \case
        'λ' -> '\\'
        c -> c
    )

saveGlobal :: Handle -> (String, Expression) -> IO ()
saveGlobal h (n, e) = hPutStrLn h (n ++ " = " ++ removeLambda (show e))

convertToName :: Environment -> Expression -> String
convertToName [] exp = findNumeral exp
convertToName ((v, e) : rest) ex
  | alphaEquiv e ex = v
  | otherwise = convertToName rest ex

convertToNames :: Bool -> Bool -> Expression -> Environment -> Expression -> String
convertToNames redexFound redexVarFind redexVar env (Variable v) =
  if redexVarFind && (Variable v == redexVar)
    then "\x1b[0;31m" ++ show v ++ "\x1b[0m"
    else show v
convertToNames redexFound redexVarFind redexVar env redex@(Application (Abstraction v e) n) =
  if redexFound
    then
      let redex1 = convertToName env redex
       in if redex1 == "none"
            then "(" ++ convertToNames True False redexVar env (Abstraction v e) ++ " " ++ convertToNames True False redexVar env n ++ ")"
            else redex1
    else "\x1b[0;35m(\x1b[1;36m(λ\x1b[1;31m" ++ show v ++ "\x1b[1;36m.\x1b[0;36m " ++ convertToNames True True (Variable v) env e ++ "\x1b[1;36m) \x1b[1;32m" ++ convertToNames True False redexVar env n ++ "\x1b[0;35m)\x1b[0m"
convertToNames redexFound redexVarFind redexVar env app@(Application m n) =
  let app1 = convertToName env app
   in if app1 == "none"
        then "(" ++ convertToNames redexFound redexVarFind redexVar env m ++ " " ++ convertToNames redexFound redexVarFind redexVar env n ++ ")"
        else app1
convertToNames redexFound redexVarFind redexVar env abs@(Abstraction v e) =
  let abs1 = convertToName env abs
   in if abs1 == "none"
        then "(λ" ++ show v ++ ". " ++ convertToNames redexFound redexVarFind redexVar env e ++ ")"
        else abs1

-- same as convertToNames, but with additional coloring meant for beta nf terms mostly
---------------------------------------------------------------------------------------------------
convertToNamesResult :: Bool -> Bool -> Expression -> Environment -> Expression -> String
convertToNamesResult redexFound redexVarFind redexVar env (Variable v) =
  if redexVarFind && (Variable v == redexVar)
    then "\x1b[0;31m" ++ show v ++ "\x1b[0m"
    else show v
convertToNamesResult redexFound redexVarFind redexVar env redex@(Application (Abstraction v e) n) =
  if redexFound
    then
      let redex1 = convertToName env redex
       in if redex1 == "none"
            then "(" ++ convertToNamesResult True False redexVar env (Abstraction v e) ++ " " ++ convertToNamesResult True False redexVar env n ++ ")"
            else redex1
    else "\x1b[0;35m(\x1b[1;36m(λ\x1b[1;31m" ++ show v ++ "\x1b[1;36m.\x1b[0;36m " ++ convertToNamesResult True True (Variable v) env e ++ "\x1b[1;36m) \x1b[1;32m" ++ convertToNamesResult True False redexVar env n ++ "\x1b[0;35m)\x1b[0m"
convertToNamesResult redexFound redexVarFind redexVar env app@(Application m n) =
  let app1 = convertToName env app
   in if app1 == "none"
        then "\x1b[0;35m(\x1b[0m" ++ convertToNamesResult redexFound redexVarFind redexVar env m ++ " " ++ convertToNamesResult redexFound redexVarFind redexVar env n ++ "\x1b[0;35m)\x1b[0m"
        else "\x1b[1;32m" ++ app1 ++ "\x1b[0m"
convertToNamesResult redexFound redexVarFind redexVar env abs@(Abstraction v e) =
  let abs1 = convertToName env abs
   in if abs1 == "none"
        then "\x1b[0;36m(\x1b[1;36mλ\x1b[0m" ++ show v ++ "\x1b[1;36m.\x1b[0m " ++ convertToNamesResult redexFound redexVarFind redexVar env e ++ "\x1b[0;36m)\x1b[0m"
        else "\x1b[1;32m" ++ abs1 ++ "\x1b[0m"

-----------------------------------------------------------------------------------------------------------
isDefined :: Environment -> String -> Bool
isDefined [] s = False
isDefined ((v, e) : rest) s
  | v == s = True
  | otherwise = isDefined rest s

reviewVariable :: Environment -> String -> String
reviewVariable [] var = "none"
reviewVariable ((v, e) : rest) var
  | v == var = show e
  | otherwise = reviewVariable rest var

-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
-- construct Expression for numeral from num and check equality
---- before was checking alpha equivalence, but we restrict it now
---- numerals will always be with same variables
---- reduces execution time, esspecially for Churchs
findChurch :: Expression -> Int -> String
findChurch exp num
  | exp == createChurch num (Variable (LambdaVar 'x' 0)) = show num
  | num == 199 = "none"
  | otherwise = findChurch exp (num + 1)

findBinary :: Expression -> Int -> String
findBinary exp num
  | exp == fst (betaNF None 0 (createBinary num)) = show num ++ "b"
  | num == 2047 = "none"
  | otherwise = findBinary exp (num + 1)

findNumeral :: Expression -> String
findNumeral abs@(Abstraction (LambdaVar v n) e)
  | v == 'f' = findChurch abs 0
  | v == 'p' = findBinary abs 0
  | otherwise = "none"
findNumeral exp = "none"

-------------------------------------------------------------------------------------
goodCounter :: Int -> Int -> Int
goodCounter num 0 = num
goodCounter _ rednum = rednum

-------------------------------------------------------------------------------------
showResult :: Environment -> EvaluateOption -> Expression -> Int -> String
showResult env evop exp num =
  let expnf = betaNF evop 0 exp
      count = goodCounter num (snd expnf)
   in showSummary env (fst expnf) count

showProgResult :: Environment -> EvaluateOption -> Expression -> Int -> String
showProgResult env evop exp num =
  let expnf = betaNF evop 0 exp
      count = goodCounter num (snd expnf)
   in showSummary env (fst expnf) count

showSummary :: Environment -> Expression -> Int -> String
showSummary env exp count =
  "\x1b[1;32m|> \x1b[0;33mreductions count               : \x1b[1;31m" ++ show count ++ "\n"
    ++ "\x1b[1;32m|> \x1b[0;33muncurried \x1b[1;33mβ-normal\x1b[0;33m form        : \x1b[0m"
    ++ show exp
    ++ "\n"
    ++ "\x1b[1;32m|> \x1b[0;33mcurried (partial) \x1b[1;33mα-equivalent\x1b[0;33m : \x1b[0m"
    ++ convertToNamesResult False False (Variable (LambdaVar '.' 0)) env exp

manualReduce :: Environment -> EvaluateOption -> Expression -> Int -> InputT IO ()
manualReduce env evop exp num = do
  outputStrLn ("\x1b[1;35m#" ++ show num ++ ":\x1b[0m" ++ convertToNames False False (Variable (LambdaVar '.' 0)) env exp)
  line <- getInputLine "\x1b[1;33mNext step?\x1b[0m [Y/n/f] (f: finish all remaining steps): "
  case line of
    Just "n" ->
      outputStrLn $ showSummary env exp num
    Just "f" -> autoReduce env evop exp num
    _ ->
      if hasBetaRedex exp
        then uncurry (manualReduce env evop) (betaReduction evop num exp)
        else outputStrLn $ showResult env evop exp num

autoReduce :: Environment -> EvaluateOption -> Expression -> Int -> InputT IO ()
autoReduce env evop exp num = do
  outputStrLn ("\x1b[1;35m#" ++ show num ++ ":\x1b[0m " ++ convertToNames False False (Variable (LambdaVar '.' 0)) env exp)
  if hasBetaRedex exp
    then uncurry (autoReduce env evop) (betaReduction evop num exp)
    else outputStrLn $ showResult env evop exp num

autoProgReduce :: Environment -> EvaluateOption -> Expression -> Int -> IO ()
autoProgReduce env evop exp num = do
  putStrLn ("#\x1b[1;35m" ++ show num ++ ":\x1b[0m " ++ convertToNames False False (Variable (LambdaVar '.' 0)) env exp)
  if hasBetaRedex exp
    then uncurry (autoProgReduce env evop) (betaReduction evop num exp)
    else putStrLn $ showProgResult env evop exp num

-------------------------------------------------------------------------------------

decideEvaluate :: Environment -> EvaluateOption -> EvaluateOption -> Expression -> InputT IO Environment
decideEvaluate env None None e = do
  let (res, env') = evalExp e `runState` env
  outputStrLn $ case res of
    Left err -> show err
    Right exp -> showResult env None exp 0
  return env
decideEvaluate env Detailed None e = do
  let (res, env') = evalExp e `runState` env
  case res of
    Left err -> outputStrLn $ show err
    Right exp -> do
      op <- getInputLine "\x1b[1;33mChoose stepping option\x1b[0m ([default] a: auto all, m: manual step-by-step): "
      case op of
        Just "a" -> autoReduce env None exp 0
        Just "m" -> manualReduce env None exp 0
        _ -> autoReduce env None exp 0
  return env
decideEvaluate env None CallByValue e = do
  let (res, env') = evalExp e `runState` env
  outputStrLn $ case res of
    Left err -> show err
    Right exp -> showResult env CallByValue exp 0
  return env
decideEvaluate env Detailed CallByValue e = do
  let (res, env') = evalExp e `runState` env
  case res of
    Left err -> outputStrLn $ show err
    Right exp -> do
      op <- getInputLine "\x1b[1;33mChoose stepping option\x1b[0m ([default] a: auto all, m: manual step-by-step): "
      case op of
        Just "a" -> autoReduce env CallByValue exp 0
        Just "m" -> manualReduce env CallByValue exp 0
        _ -> autoReduce env CallByValue exp 0
  return env

decideEvaluateProg :: Environment -> EvaluateOption -> EvaluateOption -> Expression -> IO Environment
decideEvaluateProg env None None e = do
  let (res, env') = evalExp e `runState` env
  putStrLn $ case res of
    Left err -> show err
    Right exp -> showProgResult env None exp 0
  return env
decideEvaluateProg env Detailed None e = do
  let (res, env') = evalExp e `runState` env
  case res of
    Left err -> print err
    Right exp -> autoProgReduce env None exp 0
  return env
decideEvaluateProg env None CallByValue e = do
  let (res, env') = evalExp e `runState` env
  putStrLn $ case res of
    Left err -> show err
    Right exp -> showProgResult env CallByValue exp 0
  return env
decideEvaluateProg env Detailed CallByValue e = do
  let (res, env') = evalExp e `runState` env
  case res of
    Left err -> print err
    Right exp -> autoProgReduce env CallByValue exp 0
  return env
