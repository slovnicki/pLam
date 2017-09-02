import Syntax
import Parser
import Eval

import Control.Monad.State
import System.IO (hFlush, stdout)
import Debug.Trace

execute :: String -> Environment -> IO Environment
execute line env =
    case readLine line of
        Left (SyntaxError e) -> do
            putStrLn $ show e
            return env
        Right c -> do
            case c of 
                Assign v e -> do 
                    let (res, env') = (evalAssign v e) `runState` env
                    case res of
                        Left err -> putStrLn $ show err
                        Right f -> putStrLn ("- added " ++ show f ++ " to environment as " ++ show v)
                    return env'
                Execute e -> do
                    let (res, env') = (evalE e) `runState` env
                    case res of
                        Left err -> putStrLn $ show err
                        Right f -> putStrLn ("----- result: " ++ show f)
                    return env
                Import f -> do
                    contents <- readFile f
                    let exprs = lines contents
                    putStrLn ("- imported all from " ++ show f)
                    return $ execAll exprs env

execAll :: [String] -> Environment -> Environment
execAll lines env = foldl exec env lines  where
    exec env line = case readExpr line of
        (Left err) -> env
        (Right ex) -> do
            case ex of
                Assign v e -> snd $ (evalAssign v e) `runState` env


importFile :: String -> Environment -> IO Environment
importFile filename env = do
    contents <- readFile filename
    let exprs = lines contents
    return $ execAll exprs env

-- MAIN with Read-Evaluate-Print Loop --
main :: IO ()
main = do
    repl [] where
        repl env = do
            putStr "LCI> "
            hFlush stdout
            line <- getLine
            env' <- execute line env
            repl env'
