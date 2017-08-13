import Syntax
import Parser

import Control.Monad.State
import System.IO (hFlush, stdout)

execute :: String -> Environment -> IO Environment
execute line env =
    case readExpr line of
        Left (SyntaxError e) -> do
            putStrLn $ show e
            return env
        Right f -> do
            putStrLn $ show f
            return env
            {-let (res, env') = eval f `runState` env
            case res of
                Left e -> putStrLn $ show e
                Right f -> putStrLn $ show f
            return env'-}

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
