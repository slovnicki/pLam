import Syntax
import Parser
import Evaluator
import Reducer
import Helper

import Control.Monad.State
import System.IO (hFlush, stdout)
import Debug.Trace
import System.Exit
import System.Console.Haskeline


-------------------------------------------------------------------------------------
isDefine :: String -> Bool
isDefine [] = False
isDefine (char:cs)
    | char == '=' = True
    | otherwise   = isDefine cs

execAll :: [String] -> Environment -> InputT IO Environment
execAll [] env = return env
execAll (line:ls) env =
    case isDefine line of
        True -> case readDefine line of
            Left (SyntaxError e) -> do
                outputStrLn $ (show e ++ "\ntrying to define? correct syntax is '<String> = <Expression>'")
                return env
            Right (Define v e) -> do
                let (res, env') = (evalDefine v e) `runState` env
                case res of
                    Left err -> do
                        outputStrLn (show err)
                        execAll ls env'
                    Right f  -> execAll ls env' 
        False -> case readLine line of
            Left (SyntaxError err) -> do
                outputStrLn (show err) 
                return env
            Right comm -> case comm of   
                Import f -> do
                    content <- liftIO $ readFile ("import/" ++ f ++ ".txt")
                    let exprs = lines content
                    env' <- execAll exprs env
                    execAll ls env'
                Show e -> do
                    let (res, env') = (evalExp e) `runState` env
                    case res of
                        Left err -> do
                            outputStrLn (show err)
                            return env
                        Right exp -> do
                            --putStrLn ("----- original term : " ++ show exp)
                            showResult env exp
                            execAll ls env'
                Print s -> do
                    outputStrLn s
                    execAll ls env
                otherwise -> execAll ls env

execute :: String -> Environment -> InputT IO Environment
execute line env = 
    case isDefine line of
        True -> case readDefine line of
            Left (SyntaxError e) -> do
                outputStrLn $ (show e ++ "\ntrying to define? correct syntax is '<String> = <Expression>'")
                return env
            Right (Define v e) -> do
                let (res, env') = (evalDefine v e) `runState` env
                case res of
                    Left err -> outputStrLn $ show err
                    Right f  -> outputStr "" 
                return env'
        False -> case readLine line of
            Left (SyntaxError e) -> do
                outputStrLn $ show e
                return env
            Right comm -> case comm of 
                Show e -> do
                    let (res, env') = (evalExp e) `runState` env
                    case res of
                        Left err -> outputStrLn $ show e
                        Right exp -> showResult env exp
                    return env
                ShowDetailed e -> do
                    let (res, env') = (evalExp e) `runState` env
                    case res of
                        Left err -> outputStrLn $ show e
                        Right exp -> do
                            --putStrLn ("----- original term : " ++ show exp)
                            op <- getInputLine "- type reduction option (a-auto, m-manual, t-tree, [DEFAULT-fast]): "
                            case op of
                                Just "a" -> autoReduce env exp 0
                                Just "m" -> manualReduce env exp 0
                                Just "t" -> drawPossibleReductions exp
                                otherwise -> showResult env exp
                    return env
                Import f -> do
                    content <- liftIO $ readFile ("import/" ++ f ++ ".txt")
                    let exprs = lines content
                    execAll exprs env                  
                Review r -> do
                    case r of
                       "all" -> do
                           outputStrLn " ENVIRONMENT:"
                           mapM_ showGlobal env
                       otherwise -> outputStrLn ("--- definition of " ++ show r ++ ": " ++ reviewVariable env r)
                    return env
                Run f -> do
                    content <- liftIO $ readFile f
                    let exprs = lines content
                    execAll exprs env
                Print s -> do
                    outputStrLn s
                    outputStrLn ("(NOTE: it makes more sense to use a comment line (starts with double '-' than :print command when you are in interactive mode)")
                    return env
                Comment c -> return env
                    

-------------------------------------------------------------------------------------
                   -- MAIN with Read-Evaluate-Print Loop --
-------------------------------------------------------------------------------------
main :: IO ()
main = runInputT defaultSettings (repl []) 
    where
        repl env = do
            mline <- getInputLine "LCI> "
            case mline of
                Nothing -> return ()
                Just ":quit" -> do
                    outputStrLn "Goodbye!"
                    return ()
                Just line -> do
                    env' <- execute line env
                    repl env'
