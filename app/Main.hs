import Config
import Control.Monad.State
import Debug.Trace
import Evaluator
import Helper
import Parser
import Reducer
import Syntax
import System.Console.Haskeline
import System.Directory (doesFileExist)
import System.Environment
import System.Exit
import System.IO (IOMode (WriteMode), hClose, hFlush, hPutStrLn, openFile, stdout)

version = "2.2.1"

heading =
  boldCyan
    "         _\n\
    \        | |\n\
    \    ____| |   ___  __  __\n\
    \    | _ \\ |__| _ \\|  \\/  |\n\
    \    |  _/____|____\\_\\__/_| "
    ++ boldGreen
      ( "v"
          ++ version
          ++ "\n    "
      )
    ++ boldCyan "|_| "
    ++ "pure λ-calculus interpreter\n\
       \   "
    ++ boldCyan "=================================\n"

-------------------------------------------------------------------------------------

execAll :: [String] -> Environment -> InputT IO Environment
execAll [] env = return env
execAll (line : ls) env =
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
      Define v e ->
        let (res, env') = evalDefine v e `runState` env
         in case res of
              Left err -> do
                outputStrLn (show err)
                execAll ls env'
              Right f -> execAll ls env'
      Evaluate det cbv e -> decideEvaluate env det cbv e
      Print s -> do
        outputStrLn s
        execAll ls env
      _ -> execAll ls env

execute :: String -> Environment -> InputT IO Environment
execute line env =
  case readLine line of
    Left (SyntaxError e) -> do
      outputStrLn (show e)
      return env
    Right comm -> case comm of
      Define v e -> do
        let (res, env') = evalDefine v e `runState` env
        case res of
          Left err -> outputStrLn (show err)
          Right exp -> outputStr ""
        return env'
      Evaluate det cbv e -> decideEvaluate env det cbv e
      Import f -> do
        fileExists <- liftIO $ doesFileExist (importPath ++ f ++ ".plam")
        if fileExists
          then do
            content <- liftIO $ readFile (importPath ++ f ++ ".plam")
            let exprs = lines content
            execAll exprs env
          else do
            outputStrLn ("--- import failed : " ++ f ++ ".plam does not exist within import/")
            return env
      Export f -> do
        fileExists <- liftIO $ doesFileExist (importPath ++ f ++ ".plam")
        if not fileExists
          then do
            outFile <- liftIO $ openFile (importPath ++ f ++ ".plam") WriteMode
            liftIO $ mapM_ (saveGlobal outFile) (reverse env)
            liftIO $ hClose outFile
            outputStrLn ("--- successfully exported to import/" ++ f ++ ".plam")
          else outputStrLn ("--- export failed : " ++ f ++ " already exists")
        return env
      Review r -> do
        case r of
          "all" ->
            outputStrLn " ENVIRONMENT:"
              >> mapM_ showGlobal env
          _ -> outputStrLn ("--- definition of " ++ show r ++ ": " ++ reviewVariable env r)
        return env
      Run f -> do
        content <- liftIO $ readFile (f ++ ".plam")
        let exprs = lines content
        execAll exprs env
      Print s -> do
        outputStrLn s
        outputStrLn "(NOTE: it makes more sense to use a comment line (starts with double '-' than :print command when you are in interactive mode)"
        return env
      Comment c -> return env

execJustProg :: [String] -> Environment -> IO Environment
execJustProg [] env = return env
execJustProg (line : ls) env =
  case readLine line of
    Left (SyntaxError err) -> do
      print err
      return env
    Right comm -> case comm of
      Import f -> do
        content <- liftIO $ readFile (importPath ++ f ++ ".plam")
        let exprs = lines content
        env' <- execJustProg exprs env
        execJustProg ls env'
      Define v e ->
        let (res, env') = evalDefine v e `runState` env
         in case res of
              Left err -> do
                print err
                execJustProg ls env'
              Right f -> execJustProg ls env'
      Evaluate det cbv e -> decideEvaluateProg env det cbv e
      Review r -> do
        case r of
          "all" -> do
            putStrLn " ENVIRONMENT:"
            mapM_ printGlobal env
          _ -> putStrLn ("--- definition of " ++ show r ++ ": " ++ reviewVariable env r)
        execJustProg ls env
      Print s -> do
        putStrLn s
        execJustProg ls env
      _ -> execJustProg ls env

-------------------------------------------------------------------------------------
isplam :: String -> Bool
isplam (c : cs)
  | (length cs == 5) && (cs == ".plam") = True
  | length cs < 5 = False
  | otherwise = isplam cs

-------------------------------------------------------------------------------------
-- MAIN with Read-Evaluate-Print Loop --
-------------------------------------------------------------------------------------
repl env = do
  mline <- getInputLine $ boldCyan "pLam>" ++ " "
  case mline of
    Nothing -> return ()
    Just line
      | line == ":quit" || line == ":q" -> do
        outputStrLn $ boldCyan "Goodbye!"
        return ()
      | otherwise -> do
        env' <- execute line env
        repl env'

decideRun :: [String] -> IO ()
decideRun args
  | null args = do
    putStrLn heading
    runInput
  | (length args == 1) && (head args == ":nohead") = do
    runInput
  | (length args == 1) && isplam (head args) = do
    content <- readFile (head args)
    let exprs = lines content
    execJustProg exprs []
    putStrLn $ boldGreen "Done."
  | otherwise = do
    putStrLn $ red "ignoring argument(s)..."
    putStrLn heading
    runInput
  where
    runInput = runInputT defaultSettings {historyFile = Just ".plam-history"} (repl [])

main :: IO ()
main = do
  args <- getArgs
  decideRun args
