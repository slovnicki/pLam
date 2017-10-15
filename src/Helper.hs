module Helper where

import Syntax
import Reducer

import Control.Monad.State
import System.IO (hFlush, stdout)
import Debug.Trace


-------------------------------------------------------------------------------------
showGlobal :: (String, Expression) -> IO ()
showGlobal (n, e) = putStrLn ("--- " ++ show n ++ " = " ++ show e)

convertToName :: Environment -> Expression -> String
convertToName [] ex = "none"
convertToName ((v,e):rest) ex 
    | alphaEquiv e ex = show v
    | otherwise       = convertToName rest ex

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
id' = Abstraction (LambdaVar 'x' 0) (Variable (LambdaVar 'x' 0))

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
                    case num>=100 of
                        True -> "none less than 100"
                        False -> findNumeral (betaReduction exp) (num+1)
                False -> "none" 
-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
showResult :: Environment -> Expression -> IO ()
showResult env exp = do
    let bnf = betaNF exp
    putStrLn ("----- β normal form : " ++ show bnf)
    putStrLn ("----- α-equivalent  : " ++ convertToName env bnf)
    putStrLn ("----- Church numeral: " ++ findNumeral (Application bnf id') 0)
    

manualReduce :: Environment -> Expression -> Int -> IO ()
manualReduce env exp num = do
    putStrLn ("-- " ++ show num ++ ": " ++ show exp)
    putStrLn ("Continue? [Y/n]") 
    hFlush stdout
    line <- getLine
    case line of
        "n" -> showResult env exp
        otherwise -> do
            let e2 = betaReduction exp
            case (hasBetaRedex exp) of
                True -> manualReduce env e2 (num+1)
                False -> do
                    putStrLn ("--- no beta redexes!")
                    manualReduce env e2 (num+1)

autoReduce :: Environment -> Expression -> Int -> IO ()
autoReduce env exp num = do
    putStrLn ("-- " ++ show num ++ ": " ++ show exp)
    case (hasBetaRedex exp) of
        True -> do
            case num>1000 of
                True  -> do 
                    putStrLn ("--- 1000 reductions limit!")
                    showResult env exp
                False -> do
                    let e2 = betaReduction exp
                    autoReduce env e2 (num+1)
        False -> do
            putStrLn ("--- no beta redexes!") 
            showResult env exp
-------------------------------------------------------------------------------------
