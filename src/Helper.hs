module Helper where

import Syntax
import Reducer
import Parser
import Evaluator

import Control.Monad.State
import System.IO (hFlush, stdout, Handle, hPutStrLn)
import Debug.Trace
import System.Console.Haskeline


-------------------------------------------------------------------------------------
showGlobal :: (String, Expression) -> InputT IO ()
showGlobal (n, e) = outputStrLn ("--- " ++ show n ++ " = " ++ show e)

printGlobal :: (String, Expression) -> IO ()
printGlobal (n, e) = putStrLn ("--- " ++ show n ++ " = " ++ show e)

removeLambda :: String -> String
removeLambda target =
    let
        repl 'λ' = '\\'
        repl  c  =  c
    in map repl target
    
saveGlobal :: Handle -> (String, Expression) -> IO ()
saveGlobal h (n, e) = hPutStrLn h (n ++ " = " ++ (removeLambda (show e)))

convertToName :: Environment -> Expression -> String
convertToName [] exp = findNumeral exp
convertToName ((v,e):rest) ex 
    | alphaEquiv e ex = v
    | otherwise       = convertToName rest ex

convertToNames :: Bool -> Bool -> Expression -> Environment -> Expression -> String
convertToNames redexFound redexVarFind redexVar env (Variable v) = 
    case redexVarFind of
        True -> case ((Variable v) == redexVar) of
            True -> "\x1b[0;31m" ++ (show v) ++ "\x1b[0m"
            False -> show v
        False -> show v
convertToNames redexFound redexVarFind redexVar env redex@(Application (Abstraction v e) n) = 
    case redexFound of
        True -> do
            let redex1 = convertToName env redex
            case redex1 of
                "none" -> "(" ++ (convertToNames True False redexVar env (Abstraction v e)) ++ " " ++ (convertToNames True False redexVar env n) ++ ")"
                otherwise -> redex1
        False -> "\x1b[0;35m(\x1b[1;36m(λ\x1b[1;31m" ++ (show v) ++ "\x1b[1;36m.\x1b[0;36m " ++ (convertToNames True True (Variable v) env e) ++ "\x1b[1;36m) \x1b[1;32m" ++ (convertToNames True False redexVar env n) ++ "\x1b[0;35m)\x1b[0m"
convertToNames redexFound redexVarFind redexVar env app@(Application m n) = do
    let app1 = convertToName env app
    case app1 of
        "none" -> "(" ++ (convertToNames redexFound redexVarFind redexVar env m) ++ " " ++ (convertToNames redexFound redexVarFind redexVar env n) ++ ")"
        otherwise -> app1
convertToNames redexFound redexVarFind redexVar env abs@(Abstraction v e) = do
    let abs1 = convertToName env abs
    case abs1 of
        "none" -> "(λ" ++ (show v) ++ ". " ++ (convertToNames redexFound redexVarFind redexVar env e) ++ ")"
        otherwise -> abs1


-- same as convertToNames, but with additional coloring meant for beta nf terms mostly 
---------------------------------------------------------------------------------------------------
convertToNamesResult :: Bool -> Bool -> Expression -> Environment -> Expression -> String
convertToNamesResult redexFound redexVarFind redexVar env (Variable v) = 
    case redexVarFind of
        True -> case ((Variable v) == redexVar) of
            True -> "\x1b[0;31m" ++ (show v) ++ "\x1b[0m"
            False -> show v
        False -> show v
convertToNamesResult redexFound redexVarFind redexVar env redex@(Application (Abstraction v e) n) = 
    case redexFound of
        True -> do
            let redex1 = convertToName env redex
            case redex1 of
                "none" -> "(" ++ (convertToNamesResult True False redexVar env (Abstraction v e)) ++ " " ++ (convertToNamesResult True False redexVar env n) ++ ")"
                otherwise -> redex1
        False -> "\x1b[0;35m(\x1b[1;36m(λ\x1b[1;31m" ++ (show v) ++ "\x1b[1;36m.\x1b[0;36m " ++ (convertToNamesResult True True (Variable v) env e) ++ "\x1b[1;36m) \x1b[1;32m" ++ (convertToNamesResult True False redexVar env n) ++ "\x1b[0;35m)\x1b[0m"
convertToNamesResult redexFound redexVarFind redexVar env app@(Application m n) = do
    let app1 = convertToName env app
    case app1 of
        "none" -> "\x1b[0;35m(\x1b[0m" ++ (convertToNamesResult redexFound redexVarFind redexVar env m) ++ " " ++ (convertToNamesResult redexFound redexVarFind redexVar env n) ++ "\x1b[0;35m)\x1b[0m"
        otherwise -> "\x1b[1;32m" ++ app1 ++ "\x1b[0m"
convertToNamesResult redexFound redexVarFind redexVar env abs@(Abstraction v e) = do
    let abs1 = convertToName env abs
    case abs1 of
        "none" -> "\x1b[0;36m(\x1b[1;36mλ\x1b[0m" ++ (show v) ++ "\x1b[1;36m.\x1b[0m " ++ (convertToNamesResult redexFound redexVarFind redexVar env e) ++ "\x1b[0;36m)\x1b[0m"
        otherwise -> "\x1b[1;32m" ++ abs1 ++ "\x1b[0m"

-----------------------------------------------------------------------------------------------------------
isDefined :: Environment -> String -> Bool
isDefined [] s = False
isDefined ((v,e):rest) s
    | v == s    = True
    | otherwise = isDefined rest s

reviewVariable :: Environment -> String -> String
reviewVariable [] var = "none"
reviewVariable ((v,e):rest) var
    | v == var  = show e
    | otherwise = reviewVariable rest var
-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
-- construct Expression for numeral from num and check equality
---- before was checking alpha equivalence, but we restrict it now
---- numerals will always be with same variables
---- reduces execution time, esspecially for Churchs
findChurch :: Expression -> Int -> String
findChurch exp num = do
    case (exp == (createChurch num (Variable (LambdaVar 'x' 0)))) of
        True -> show num
        False -> do
            case num==199 of
                True -> "none"
                False -> findChurch exp (num+1)

findBinary :: Expression -> Int -> String
findBinary exp num = do
    case exp == fst ((betaNF None 0 (createBinary num))) of
        True -> (show num) ++ "b"
        False -> do
            case num==2047 of
                True -> "none"
                False -> findBinary exp (num+1)

findNumeral :: Expression -> String 
findNumeral abs@(Abstraction (LambdaVar v n) e)
    | v == 'f'  = findChurch abs 0
    | v == 'p'  = findBinary abs 0
    | otherwise = "none"
findNumeral exp = "none"


-------------------------------------------------------------------------------------
goodCounter :: Int -> Int -> Int
goodCounter num rednum | rednum==0 = num
                       | otherwise = rednum

-------------------------------------------------------------------------------------
showResult :: Environment -> EvaluateOption -> Expression -> Int -> String
showResult env evop exp num =
    let expnf = betaNF evop 0 exp
        count = goodCounter num (snd expnf)
    in
        showSummary env (fst expnf) count

showProgResult :: Environment -> EvaluateOption -> Expression -> Int -> String
showProgResult env evop exp num =
    let expnf = betaNF evop 0 exp
        count = goodCounter num (snd expnf)
    in
        showSummary env (fst expnf) count

showSummary :: Environment -> Expression -> Int -> String
showSummary env exp count =
    "\x1b[1;32m|> \x1b[0;33mreductions count               : \x1b[1;31m" ++ show count ++ "\n" ++
    "\x1b[1;32m|> \x1b[0;33muncurried \x1b[1;33mβ-normal\x1b[0;33m form        : \x1b[0m" ++ show exp ++ "\n" ++
    "\x1b[1;32m|> \x1b[0;33mcurried (partial) \x1b[1;33mα-equivalent\x1b[0;33m : \x1b[0m" ++ convertToNamesResult False False (Variable (LambdaVar '.' 0)) env exp


manualReduce :: Environment -> EvaluateOption -> Expression -> Int -> InputT IO ()
manualReduce env evop exp num = do 
    outputStrLn ("\x1b[1;35m#" ++ show num ++ ":\x1b[0m" ++ (convertToNames False False (Variable (LambdaVar '.' 0)) env exp))
    line <- getInputLine "\x1b[1;33mNext step?\x1b[0m [Y/n/f] (f: finish all remaining steps): "
    case line of
        Just "n" ->
            outputStrLn $ showSummary env exp num
        Just "f" -> autoReduce env evop exp num
        otherwise ->
            case (hasBetaRedex exp) of
                True ->
                    let e2b = betaReduction evop num exp
                    in manualReduce env evop (fst e2b) (snd e2b)
                False ->
                    outputStrLn $ showResult env evop exp num


autoReduce :: Environment -> EvaluateOption -> Expression -> Int -> InputT IO ()
autoReduce env evop exp num = do
    outputStrLn ("\x1b[1;35m#" ++ show num ++ ":\x1b[0m " ++ (convertToNames False False (Variable (LambdaVar '.' 0)) env exp))
    case (hasBetaRedex exp) of
        True -> do
            let e2b = betaReduction evop num exp
            autoReduce env evop (fst e2b) (snd e2b)        
        False -> do
           outputStrLn $ showResult env evop exp num
            
autoProgReduce :: Environment -> EvaluateOption -> Expression -> Int -> IO ()
autoProgReduce env evop exp num = do
    putStrLn ("#\x1b[1;35m" ++ show num ++ ":\x1b[0m " ++ (convertToNames False False (Variable (LambdaVar '.' 0)) env exp))
    case (hasBetaRedex exp) of
        True -> do
            let e2b = betaReduction evop num exp
            autoProgReduce env evop (fst e2b) (snd e2b)        
        False -> do
            putStrLn $ showProgResult env evop exp num

-------------------------------------------------------------------------------------

decideEvaluate :: Environment -> EvaluateOption -> EvaluateOption -> Expression -> InputT IO Environment
decideEvaluate env None None e = do
    let (res, env') = (evalExp e) `runState` env
    case res of
        Left err -> outputStrLn $ show err
        Right exp -> outputStrLn $ showResult env None exp 0
    return env
decideEvaluate env Detailed None e = do
    let (res, env') = (evalExp e) `runState` env
    case res of
        Left err -> outputStrLn $ show err
        Right exp -> do
            op <- getInputLine "\x1b[1;33mChoose stepping option\x1b[0m ([default] a: auto all, m: manual step-by-step): "
            case op of
                Just "a" -> autoReduce env None exp 0
                Just "m" -> manualReduce env None exp 0
                otherwise -> autoReduce env None exp 0
    return env
decideEvaluate env None CallByValue e = do
    let (res, env') = (evalExp e) `runState` env
    case res of
        Left err -> outputStrLn $ show err
        Right exp -> outputStrLn $ showResult env CallByValue exp 0
    return env
decideEvaluate env Detailed CallByValue e = do
    let (res, env') = (evalExp e) `runState` env
    case res of
        Left err -> outputStrLn $ show err
        Right exp -> do
            op <- getInputLine "\x1b[1;33mChoose stepping option\x1b[0m ([default] a: auto all, m: manual step-by-step): "
            case op of
                Just "a" -> autoReduce env CallByValue exp 0
                Just "m" -> manualReduce env CallByValue exp 0
                otherwise -> autoReduce env CallByValue exp 0
    return env

decideEvaluateProg :: Environment -> EvaluateOption -> EvaluateOption -> Expression -> IO Environment
decideEvaluateProg env None None e = do
    let (res, env') = (evalExp e) `runState` env
    case res of
        Left err -> putStrLn $ show err
        Right exp -> putStrLn $ showProgResult env None exp 0
    return env
decideEvaluateProg env Detailed None e = do
    let (res, env') = (evalExp e) `runState` env
    case res of
        Left err -> putStrLn $ show err
        Right exp -> autoProgReduce env None exp 0
    return env
decideEvaluateProg env None CallByValue e = do
    let (res, env') = (evalExp e) `runState` env
    case res of
        Left err -> putStrLn $ show err
        Right exp -> putStrLn $ showProgResult env CallByValue exp 0
    return env
decideEvaluateProg env Detailed CallByValue e = do
    let (res, env') = (evalExp e) `runState` env
    case res of
        Left err -> putStrLn $ show err
        Right exp -> autoProgReduce env CallByValue exp 0
    return env
