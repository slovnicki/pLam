import Syntax
import Parser
import Evaluator
import Reductions

import Control.Monad.State
import System.IO (hFlush, stdout)
import Debug.Trace
import System.Exit


execAll :: [String] -> Environment -> Environment
execAll lines env = foldl exec env lines  where
    exec env line = case readLine line of
        Left err -> trace ("-- " ++ show err) (env)
        Right ex -> do
            case ex of
                Define v e -> snd $ (evalDefine v e) `runState` env
                Comment c ->  env

showGlobal :: (String, Expression) -> IO ()
showGlobal (n, e) = putStrLn ("--- " ++ show n ++ " = " ++ show e)

convertToName :: Environment -> Expression -> String
convertToName [] ex = "none"
convertToName ((v,e):rest) ex 
    | alphaEquiv e ex = show v
    | otherwise            = convertToName rest ex

reviewVariable :: Environment -> String -> String
reviewVariable [] var = "none"
reviewVariable ((v,e):rest) var
    | v == var  = show e
    | otherwise = reviewVariable rest var

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

showResult :: Environment -> Expression -> IO ()
showResult env exp = do
    putStrLn ("----- result        : " ++ show exp)
    putStrLn ("----- α-equivalent  : " ++ convertToName env exp)
    putStrLn ("----- Church numeral: " ++ findNumeral (Application exp id') 0)
    

manualBeta :: Environment -> Expression -> Int -> IO ()
manualBeta env exp num = do
    putStrLn ("-- " ++ show num ++ ": " ++ show exp)
    putStrLn ("Continue? [Y/n]") 
    hFlush stdout
    line <- getLine
    case line of
        "n" -> showResult env exp
        otherwise -> do
            let e2 = betaReduction exp
            case (hasBetaRedex exp) of
                True -> manualBeta env e2 (num+1)
                False -> do
                    putStrLn ("--- no beta redexes!")
                    manualBeta env e2 (num+1)

loopBeta :: Environment -> Expression -> Int -> IO ()
loopBeta env exp num = do
    putStrLn ("-- " ++ show num ++ ": " ++ show exp)
    case (hasBetaRedex exp) of
        True -> do
            case num>1000 of
                True  -> do 
                    putStrLn ("--- 1000 reductions limit!")
                    showResult env exp
                False -> do
                    let e2 = betaReduction exp
                    loopBeta env e2 (num+1)
        False -> do
            putStrLn ("--- no beta redexes!") 
            showResult env exp


execute :: String -> Environment -> IO Environment
execute line env =
    case readLine line of
        Left (SyntaxError e) -> do
            putStrLn $ show e
            return env
        Right c -> do
            case c of 
                Define v e -> do 
                    let (res, env') = (evalDefine v e) `runState` env
                    case res of
                        Left err -> putStrLn $ show err
                        Right f  -> putStr("") 
                    return env'
                Execute e -> do
                    let (res, env') = (evalExp e) `runState` env
                    case res of
                        Left err -> putStrLn $ show err
                        Right f -> do
                            putStr ("- type reduction option (a-auto, m-manual, t-tree): ") 
                            hFlush stdout
                            op <- getLine
                            case op of
                                "a"   -> loopBeta env f 0
                                "m" -> manualBeta env f 0
                                "t"   -> drawPossibleReductions f
                                otherwise -> putStrLn (" ERROR: unknown execute option " ++ show op ++ "\n- available options for execution are:\n    a\n    m\n    t")
                    return env
                Import f -> do
                    contents <- readFile ("import/" ++ f)
                    let exprs = lines contents
                    return $ execAll exprs env
                Review r -> do
                    case r of
                       "all" -> do
                           putStrLn (" ENVIRONMENT:")
                           mapM_ showGlobal env
                       otherwise -> putStrLn("--- definition of " ++ show r ++ ": " ++ reviewVariable env r)
                    return env
                Comment c -> return env
                    


-- MAIN with Read-Evaluate-Print Loop --
main :: IO ()
main = do
    repl [] where
        repl env = do
            putStr "LCI> "
            hFlush stdout
            line <- getLine
            case line of
                ":quit" -> exitSuccess
                otherwise -> do
                    env' <- execute line env
                    repl env'
