import Syntax
import Parser
import Evaluator
import Reducer
import Helper

import Control.Monad.State
import System.IO (hFlush, stdout)
import Debug.Trace
import System.Exit


-------------------------------------------------------------------------------------
execAll :: [String] -> Environment -> IO Environment
execAll [] env = do
    putStr ""
    return env
execAll (line:ls) env =
    case readLine line of
        Left (SyntaxError err) -> do
            putStrLn (show err) 
            return env
        Right ln -> do
            case ln of 
                Define v e -> do
                    let (res, env') = (evalDefine v e) `runState` env
                    case res of
                        Left err -> do
                            putStrLn (show err)
                            return env
                        Right f  -> execAll ls env'  
                Import f -> do
                    contents <- readFile ("import/" ++ f ++ ".txt")
                    let exprs = lines contents
                    env' <- execAll exprs env
                    execAll ls env'
                Execute e -> do
                    let (res, env') = (evalExp e) `runState` env
                    case res of
                        Left err -> do
                            putStrLn (show err)
                            return env
                        Right exp -> do
                            let res = betaNF exp
                            putStrLn ("----- result        : " ++ show (betaNF res) ++ "\n----- α-equivalent  : " ++ (convertToName env' res) ++ "\n----- Church numeral: " ++ (findNumeral (Application res id') 0) ++ "\n------------------------------") 
                            execAll ls env'
                otherwise -> execAll ls env

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
                        Right exp -> do
                            putStr ("- type reduction option (a-auto, m-manual, t-tree, [DEFAULT-fast]): ") 
                            hFlush stdout
                            op <- getLine
                            case op of
                                "a" -> autoReduce env exp 0
                                "m" -> manualReduce env exp 0
                                "t" -> drawPossibleReductions exp
                                otherwise -> showResult env (betaNF exp)
                    return env
                Import f -> do
                    contents <- readFile ("import/" ++ f ++ ".txt")
                    let exprs = lines contents
                    execAll exprs env
                Review r -> do
                    case r of
                       "all" -> do
                           putStrLn (" ENVIRONMENT:")
                           mapM_ showGlobal env
                       otherwise -> putStrLn("--- definition of " ++ show r ++ ": " ++ reviewVariable env r)
                    return env
                Comment c -> return env
                Run f -> do
                    contents <- readFile f
                    let exprs = lines contents
                    execAll exprs env
                    

-------------------------------------------------------------------------------------
                   -- MAIN with Read-Evaluate-Print Loop --
-------------------------------------------------------------------------------------
-- :run filename, new function for it
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
