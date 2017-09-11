import Syntax
import Parser
import Eval
import Reductions

import Control.Monad.State
import System.IO (hFlush, stdout)
import Debug.Trace
import System.Exit


execAll :: [String] -> Environment -> Environment
execAll lines env = foldl exec env lines  where
    exec env line = case readExpr line of
        Left err -> trace ("-- " ++ show err) (env)
        Right ex -> do
            case ex of
                Define v e -> snd $ (evalDefine v e) `runState` env
                Comment c ->  env

showGlobal :: (LambdaVar, Expression) -> IO ()
showGlobal (n, e) = putStrLn ("--- " ++ show n ++ " = " ++ show e)

convertToName :: Environment -> Expression -> String
convertToName [] ex = "none"
convertToName ((v,e):rest) ex 
    | alphaEquiv e ex = show v
    | otherwise            = convertToName rest ex

reviewVariable :: Environment -> LambdaVar -> String
reviewVariable [] var = "none"
reviewVariable ((v,e):rest) var
    | v == var  = show e
    | otherwise = reviewVariable rest var


manualBeta :: Environment -> Expression -> Int -> IO ()
manualBeta env exp num = do
    putStrLn ("-- " ++ show num ++ ": " ++ show exp)
    putStrLn ("Continue? [Y/n]") 
    hFlush stdout
    line <- getLine
    case line of
        "n" -> do
            putStrLn ("----- result        : " ++ show exp)
            putStrLn ("----- α-equivalent  : " ++ convertToName env exp)
            putStrLn ("----- natural number: none") -- TODO
        otherwise -> do
            let e2 = betaReduction exp
            case (e2 == exp) of
                True -> do
                    putStrLn ("-- fixed point reached!")
                    putStrLn ("----- result        : " ++ show exp)
                    putStrLn ("----- α-equivalent  : " ++ convertToName env exp)
                    putStrLn ("----- natural number: none") -- TODO
                False -> manualBeta env e2 (num+1)

loopBeta :: Environment -> Expression -> Int -> IO ()
loopBeta env exp num = do
    putStrLn ("-- " ++ show num ++ ": " ++ show exp)
    let e2 = betaReduction exp
    case (e2 == exp) of
        True -> do
            putStrLn ("-- fixed point reached!")
            putStrLn ("----- result        : " ++ show exp)
            putStrLn ("----- α-equivalent  : " ++ convertToName env exp)
            putStrLn ("----- natural number: none") -- TODO
        False -> loopBeta env e2 (num+1)

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
                Execute op e -> do
                    let (res, env') = (evalE e) `runState` env
                    case res of
                        Left err -> putStrLn $ show err
                        Right f -> do
                            case op of
                                "manual" -> manualBeta env f 0
                                "auto"   -> loopBeta env f 0
                                "tree"   -> drawPossibleReductions f
                                otherwise -> putStrLn (" ERROR: unknown option " ++ show op ++ "\n- available options for executions are:\n    manual\n    auto\n    tree")
                    return env
                Import f -> do
                    contents <- readFile ("import/" ++ f)
                    let exprs = lines contents
                    putStrLn ("- imported all from " ++ show f)
                    return $ execAll exprs env
                Review r -> do
                    case r of
                       "all" -> do
                           putStrLn (" ENVIRONMENT:")
                           mapM_ showGlobal env
                       otherwise -> putStrLn("--- definition of " ++ show r ++ ": " ++ reviewVariable env (LambdaVar (head r) 0))
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
