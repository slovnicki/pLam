import Config
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


version = "1.2.0"
-------------------------------------------------------------------------------------

execAll :: [String] -> Environment -> InputT IO Environment
execAll [] env = return env
execAll (line:ls) env =
    case readLine line of
            Left (SyntaxError err) -> do
                outputStrLn (show err) 
                return env
            Right comm -> case comm of   
                Import f -> do
                    content <- liftIO $ readFile (importPath ++ f ++ ".plam")
                    let exprs = lines content
                    env' <- execAll exprs env
                    execAll ls env'
                Define v e -> do
                    let (res, env') = (evalDefine v e) `runState` env
                    case res of
                        Left err -> do
                            outputStrLn (show err)
                            execAll ls env'
                        Right f  -> execAll ls env'
                Show e -> do
                    let (res, env') = (evalExp e) `runState` env
                    case res of
                        Left err -> do
                            outputStrLn (show err)
                            return env
                        Right exp -> do
                            --putStrLn ("----- original term : " ++ show exp)
                            showResult env exp 0
                            execAll ls env'
                Print s -> do
                    outputStrLn s
                    execAll ls env
                otherwise -> execAll ls env

execute :: String -> Environment -> InputT IO Environment
execute line env = 
    case readLine line of
            Left (SyntaxError e) -> do
                outputStrLn $ show e
                return env
            Right comm -> case comm of
                Define v e -> do
                    let (res, env') = (evalDefine v e) `runState` env
                    case res of
                        Left err -> outputStrLn (show err)
                        Right exp -> outputStr ""
                    return env'
                Show e -> do
                    let (res, env') = (evalExp e) `runState` env
                    case res of
                        Left err -> outputStrLn $ show err
                        Right exp -> showResult env exp 0
                    return env
                ShowDetailed e -> do
                    let (res, env') = (evalExp e) `runState` env
                    case res of
                        Left err -> outputStrLn $ show err
                        Right exp -> do
                            --putStrLn ("----- original term : " ++ show exp)
                            op <- getInputLine "- type reduction option (a-auto, m-manual, [DEFAULT-fast]): "
                            case op of
                                Just "a" -> autoReduce env exp 0
                                Just "m" -> manualReduce env exp 0
                                otherwise -> showResult env exp 0
                    return env
                Import f -> do
                    content <- liftIO $ readFile (importPath ++ f ++ ".plam")
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
                    content <- liftIO $ readFile (f ++ ".plam")
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
main = 
    trace ("\x1b[1;36m\
\         _\n\
\        | |\n\
\    ____| |   ___  __  __\n\
\    | _ \\ |__| _ \\|  \\/  |\n\
\    |  _/____|____\\_\\__/_| \x1b[32mv"++version++"\n\
\    \x1b[1;36m|_| \x1b[0mpure λ-calculus interpreter\n\
\   \x1b[1;36m=================================\n") (runInputT defaultSettings (repl [])) where
        repl env = do
            mline <- getInputLine "\x1b[1;36mpLam>\x1b[0m "
            case mline of
                Nothing -> return ()
                Just ":quit" -> do
                    outputStrLn "\x1b[1;32mGoodbye!\x1b[0m"
                    return ()
                Just line -> do
                    env' <- execute line env
                    repl env'
