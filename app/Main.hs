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
execAll :: [String] -> Environment -> Environment
execAll lines env = foldl exec env lines  where
    exec env line = case readLine line of
        Left err -> trace ("-- " ++ show err) (env)
        Right ex -> do
            case ex of
                Define v e -> snd $ (evalDefine v e) `runState` env
                otherwise  -> env

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
                    

-------------------------------------------------------------------------------------
                   -- MAIN with Read-Evaluate-Print Loop --
-------------------------------------------------------------------------------------
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
