module Helper where

import Syntax
import Reducer
import Parser

import Control.Monad.State
import System.IO (hFlush, stdout)
import Debug.Trace
import System.Console.Haskeline


-------------------------------------------------------------------------------------
showGlobal :: (String, Expression) -> InputT IO ()
showGlobal (n, e) = outputStrLn ("--- " ++ show n ++ " = " ++ show e)

convertToName :: Environment -> Expression -> String
convertToName [] exp = findNumeral exp
convertToName ((v,e):rest) ex 
    | alphaEquiv e ex = v
    | otherwise       = convertToName rest ex

convertToNames :: Bool -> Bool -> Expression -> Environment -> Expression -> String
convertToNames redexFound redexVarFind redexVar env (Variable v) = 
    case redexVarFind of
        True -> case ((Variable v) == redexVar) of
            True -> "\x1b[0;31m" ++ (show v) ++ "\x1b[0;36m"
            False -> show v
        False -> show v
convertToNames redexFound redexVarFind redexVar env redex@(Application (Abstraction v e) n) = 
    case redexFound of
        True -> do
            let redex1 = convertToName env redex
            case redex1 of
                "none" -> "(" ++ (convertToNames True False redexVar env (Abstraction v e)) ++ " " ++ (convertToNames True False redexVar env n) ++ ")"
                otherwise -> redex1
        False -> "\x1b[36m(\x1b[1;36m(λ\x1b[1;31m" ++ (show v) ++ "\x1b[1;36m.\x1b[0;36m " ++ (convertToNames True True (Variable v) env e) ++ "\x1b[1;36m) \x1b[1;32m" ++ (convertToNames True False redexVar env n) ++ "\x1b[0;36m)\x1b[0m"
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
    case exp == fst ((betaNF 0 (createBinary num))) of
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
showResult :: Environment -> Expression -> Int -> InputT IO ()
showResult env exp num = do
    let expnf = betaNF 0 exp
    let count = goodCounter num (snd expnf)
    outputStrLn ("\x1b[1;32m|> \x1b[0;33mreductions count               : \x1b[1;32m" ++ show count)
    outputStrLn ("\x1b[1;32m|> \x1b[0;33muncurried \x1b[1;33mβ-normal\x1b[0;33m form        : \x1b[0m" ++ show (fst expnf))
    outputStrLn ("\x1b[1;32m|> \x1b[0;33mcurried (partial) \x1b[1;33mα-equivalent\x1b[0;33m : \x1b[0m" ++ convertToNames False False (Variable (LambdaVar '.' 0)) env (fst expnf))

showProgResult :: Environment -> Expression -> Int -> IO ()
showProgResult env exp num = do
    let expnf = betaNF 0 exp
    let count = goodCounter num (snd expnf)
    putStrLn ("\x1b[1;32m|> \x1b[0;33mreductions count               : \x1b[1;32m" ++ show count)
    putStrLn ("\x1b[1;32m|> \x1b[0;33muncurried \x1b[1;33mβ-normal\x1b[0;33m form        : \x1b[0m" ++ show (fst expnf))
    putStrLn ("\x1b[1;32m|> \x1b[0;33mcurried (partial) \x1b[1;33mα-equivalent\x1b[0;33m : \x1b[0m" ++ convertToNames False False (Variable (LambdaVar '.' 0)) env (fst expnf))
    


manualReduce :: Environment -> Expression -> Int -> InputT IO ()
manualReduce env exp num = do 
    outputStrLn ("\x1b[1;35m#" ++ show num ++ ":\x1b[0m" ++ (convertToNames False False (Variable (LambdaVar '.' 0)) env exp))
    line <- getInputLine "\x1b[1;33mNext step?\x1b[0m [Y/n/f] (f: finish all remaining steps): "
    case line of
        Just "n" -> do
            outputStrLn ("\x1b[1;32m|> \x1b[0;33mreductions count               : \x1b[1;32m" ++ show num)
            outputStrLn ("\x1b[1;32m|> \x1b[0;33muncurried \x1b[1;33mβ-normal\x1b[0;33m form        : \x1b[0m" ++ show exp)
            outputStrLn ("\x1b[1;32m|> \x1b[0;33mcurried (partial) \x1b[1;33mα-equivalent\x1b[0;33m : \x1b[0m" ++ convertToNames False False (Variable (LambdaVar '.' 0)) env exp)
        Just "f" -> autoReduce env exp num
        otherwise -> do
            case (hasBetaRedex exp) of
                True -> do
                    let e2b = betaReduction num exp
                    manualReduce env (fst e2b) (snd e2b)
                False -> do
                    showResult env exp num


autoReduce :: Environment -> Expression -> Int -> InputT IO ()
autoReduce env exp num = do
    outputStrLn ("\x1b[1;35m#" ++ show num ++ ":\x1b[0m " ++ (convertToNames False False (Variable (LambdaVar '.' 0)) env exp))
    case (hasBetaRedex exp) of
        True -> do
            let e2b = betaReduction num exp
            autoReduce env (fst e2b) (snd e2b)        
        False -> do
            showResult env exp num
            
autoProgReduce :: Environment -> Expression -> Int -> IO ()
autoProgReduce env exp num = do
    putStrLn ("#\x1b[1;35m" ++ show num ++ ":\x1b[0m " ++ (convertToNames False False (Variable (LambdaVar '.' 0)) env exp))
    case (hasBetaRedex exp) of
        True -> do
            let e2b = betaReduction num exp
            autoProgReduce env (fst e2b) (snd e2b)        
        False -> do
            showProgResult env exp num

-------------------------------------------------------------------------------------
