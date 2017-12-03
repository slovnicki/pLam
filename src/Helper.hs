module Helper where

import Syntax
import Reducer

import Control.Monad.State
import System.IO (hFlush, stdout)
import Debug.Trace
import System.Console.Haskeline


id' = Abstraction (LambdaVar 'x' 0) (Variable (LambdaVar 'x' 0))

-------------------------------------------------------------------------------------
showGlobal :: (String, Expression) -> InputT IO ()
showGlobal (n, e) = outputStrLn ("--- " ++ show n ++ " = " ++ show e)

convertToName :: Environment -> Expression -> String
convertToName [] abs@(Abstraction v e) = do
    let numeral = findNumeral (Application abs id') 0
    case numeral of
        "none" -> "none"
        otherwise -> numeral
convertToName [] ex = "none"
convertToName ((v,e):rest) ex 
    | alphaEquiv e ex = v
    | otherwise       = convertToName rest ex

convertToNames :: Environment -> Expression -> String
convertToNames env (Variable v) = show v
convertToNames env app@(Application m n) = do
    --let app1 = trace ("app " ++ show m ++ " : " ++ show n) (convertToName env app)
    let app1 = convertToName env app
    case app1 of
        "none" -> "(" ++ (convertToNames env m) ++ " " ++ (convertToNames env n) ++ ")"
        otherwise -> app1
        --otherwise -> trace (" ------> " ++ app1) app1
convertToNames env abs@(Abstraction v e) = do
    --let abs1 = trace ("abs " ++ show v ++ " : " ++ show e) (convertToName env abs)
    let abs1 = convertToName env abs
    case abs1 of
        "none" -> "(λ" ++ (show v) ++ ". " ++ (convertToNames env e) ++ ")"
        otherwise -> abs1
        --otherwise -> trace (" ------> " ++ abs1) abs1

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

findNumeral :: Expression -> Int -> String
findNumeral app@(Application e1 id') num = do
    case (alphaEquiv e1 id') of
        True -> "none"
        False -> findNumeral (betaReduction app) num
findNumeral exp num = do
    case (alphaEquiv exp id') of
        True -> show num
        False -> do
            case (hasBetaRedex exp) of
                True -> do
                    case num>=10 of
                        True -> "none"
                        False -> findNumeral (betaReduction exp) (num+1)
                False -> "none" 
-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
showResult :: Environment -> Expression -> InputT IO ()
showResult env exp = do
    let bnf = betaNF exp
    outputStrLn ("----- β normal form : " ++ show bnf)
    let name = convertToName env bnf
    case name of
        "none" -> outputStr ""
        otherwise -> outputStrLn ("----- α-equivalent  : " ++ name)
    

manualReduce :: Environment -> Expression -> Int -> InputT IO ()
manualReduce env exp num = do 
    outputStrLn ("-- " ++ show num ++ ": " ++ (convertToNames env exp))
    --outputStrLn ("---- (" ++ show num ++ ": " ++ show exp ++ ")")
    line <- getInputLine "Continue? [Y/n]"
    case line of
        Just "n" -> showResult env exp
        otherwise -> do
            let e2 = betaReduction exp
            case (hasBetaRedex exp) of
                True -> manualReduce env e2 (num+1)
                False -> do
                    outputStrLn ("--- no beta redexes!")
                    manualReduce env e2 (num+1)

autoReduce :: Environment -> Expression -> Int -> InputT IO ()
autoReduce env exp num = do
    outputStrLn ("-- " ++ show num ++ ": " ++ (convertToNames env exp))
    --outputStrLn ("---- (" ++ show num ++ ": " ++ show exp ++ ")")
    case (hasBetaRedex exp) of
        True -> do
            case num>1000 of
                True  -> do 
                    outputStrLn ("--- 1000 reductions limit!")
                    showResult env exp
                False -> do
                    let e2 = betaReduction exp
                    autoReduce env e2 (num+1)
        False -> do
            outputStrLn ("--- no beta redexes!") 
            showResult env exp
-------------------------------------------------------------------------------------
