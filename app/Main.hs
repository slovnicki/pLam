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
isDefine :: String -> Bool
isDefine [] = False
isDefine (char:cs)
    | char == '=' = True
    | otherwise   = isDefine cs

execAll :: [String] -> Environment -> IO Environment
execAll [] env = return env
execAll (line:ls) env =
    case isDefine line of
        True -> case readDefine line of
            Left (SyntaxError e) -> do
                putStrLn $ (show e ++ "\ntrying to define? correct syntax is '<String> = <Expression>'")
                return env
            Right (Define v e) -> do
                let (res, env') = (evalDefine v e) `runState` env
                case res of
                    Left err -> do
                        putStrLn (show err)
                        execAll ls env'
                    Right f  -> execAll ls env' 
        False -> case readLine line of
            Left (SyntaxError err) -> do
                putStrLn (show err) 
                return env
            Right comm -> case comm of   
                Import f -> do
                    contents <- readFile ("import/" ++ f ++ ".txt")
                    let exprs = lines contents
                    env' <- execAll exprs env
                    execAll ls env'
                Show e -> do
                    let (res, env') = (evalExp e) `runState` env
                    case res of
                        Left err -> do
                            putStrLn (show err)
                            return env
                        Right exp -> do
                            --putStrLn ("----- original term : " ++ show exp)
                            showResult env exp
                            execAll ls env'
                Print s -> do
                    putStrLn s
                    execAll ls env
                otherwise -> execAll ls env

execute :: String -> Environment -> IO Environment
execute line env = 
    case isDefine line of
        True -> case readDefine line of
            Left (SyntaxError e) -> do
                putStrLn $ (show e ++ "\ntrying to define? correct syntax is '<String> = <Expression>'")
                return env
            Right (Define v e) -> do
                let (res, env') = (evalDefine v e) `runState` env
                case res of
                    Left err -> putStrLn $ show err
                    Right f  -> putStr("") 
                return env'
        False -> case readLine line of
            Left (SyntaxError e) -> do
                putStrLn $ show e
                return env
            Right comm -> case comm of 
                Show e -> do
                    let (res, env') = (evalExp e) `runState` env
                    case res of
                        Left err -> putStrLn $ show e
                        Right exp -> showResult env exp
                    return env
                ShowDetailed e -> do
                    let (res, env') = (evalExp e) `runState` env
                    case res of
                        Left err -> putStrLn $ show e
                        Right exp -> do
                            --putStrLn ("----- original term : " ++ show exp)
                            putStr ("- type reduction option (a-auto, m-manual, t-tree, [DEFAULT-fast]): ") 
                            hFlush stdout
                            op <- getLine
                            case op of
                                "a" -> autoReduce env exp 0
                                "m" -> manualReduce env exp 0
                                "t" -> drawPossibleReductions exp
                                otherwise -> showResult env exp
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
                Run f -> do
                    contents <- readFile f
                    let exprs = lines contents
                    execAll exprs env
                Print s -> do
                    putStrLn s
                    putStrLn ("(NOTE: it makes more sense to use a comment line (starts with double '-' than :print command when you are in interactive mode)")
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
