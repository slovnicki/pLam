import Config
import Control.Monad.State
import Data.List (isSuffixOf)
import Debug.Trace
import Evaluator
import Helper
import Options.Applicative
import Options.Applicative qualified as Options
import Parser
import Reducer
import Syntax
import System.Console.Haskeline
import System.Directory (doesFileExist)
import System.Environment
import System.Exit
import System.IO (IOMode (WriteMode), hClose, hFlush, hPutStrLn, openFile, stdout)

version :: String
version = "2.2.1"

heading :: String
heading =
  boldCyan
    "         _\n\
    \        | |\n\
    \    ____| |   ___  __  __\n\
    \    | _ \\ |__| _ \\|  \\/  |\n\
    \    |  _/____|____\\_\\__/_| "
    <> boldGreen
      ( "v"
          <> version
          <> "\n    "
      )
    <> boldCyan "|_| "
    <> "pure λ-calculus interpreter\n\
       \   "
    <> boldCyan "=================================\n"

-------------------------------------------------------------------------------------

execAll :: [String] -> Environment -> InputT IO Environment
execAll [] env = pure env
execAll (line : ls) env =
  case readLine line of
    Left (SyntaxError err) ->
      outputStrLn (show err)
        >> pure env
    Right comm -> case comm of
      Import f ->
        liftIO (readFile (importPath <> f <> ".plam"))
          >>= (\content -> execAll (lines content) env)
          >>= execAll ls
      Define v e ->
        let (res, env') = evalDefine v e `runState` env
         in case res of
              Left err ->
                outputStrLn (show err)
                  >> execAll ls env'
              Right f -> execAll ls env'
      Evaluate det cbv e -> decideEvaluate env det cbv e
      Print s ->
        outputStrLn s
          >> execAll ls env
      _ -> execAll ls env

execute :: String -> Environment -> InputT IO Environment
execute line env =
  case readLine line of
    Left (SyntaxError e) ->
      outputStrLn (show e)
        >> pure env
    Right comm -> case comm of
      Define v e ->
        let (res, env') = evalDefine v e `runState` env
         in ( case res of
                Left err -> outputStrLn (show err)
                Right exp -> outputStr ""
            )
              >> pure env'
      Evaluate det cbv e -> decideEvaluate env det cbv e
      Import f ->
        liftIO (doesFileExist (importPath <> f <> ".plam"))
          >>= ( \case
                  True ->
                    liftIO (readFile (importPath <> f <> ".plam"))
                      >>= (\content -> execAll (lines content) env)
                  False ->
                    outputStrLn ("--- import failed : " <> f <> ".plam does not exist within import/")
                      >> pure env
              )
      Export f ->
        liftIO (doesFileExist (importPath <> f <> ".plam"))
          >>= ( \case
                  False ->
                    ( liftIO (openFile (importPath <> f <> ".plam") WriteMode)
                        >>= ( \outFile ->
                                liftIO (mapM_ (saveGlobal outFile) (reverse env) >> hClose outFile)
                            )
                          >> outputStrLn ("--- successfully exported to import/" <> f <> ".plam")
                    )
                  True -> outputStrLn ("--- export failed : " <> f <> " already exists")
              )
          >> pure env
      Review r ->
        ( case r of
            "all" ->
              outputStrLn " ENVIRONMENT:"
                >> mapM_ showGlobal env
            _ -> outputStrLn ("--- definition of " <> show r <> ": " <> reviewVariable env r)
        )
          >> pure env
      Run f ->
        liftIO (readFile (f <> ".plam"))
          >>= (\content -> execAll (lines content) env)
      Print s ->
        outputStrLn s
          >> outputStrLn "(NOTE: it makes more sense to use a comment line (starts with double '-' than :print command when you are in interactive mode)"
          >> pure env
      Comment c -> pure env

execJustProg :: [String] -> Environment -> IO Environment
execJustProg [] env = pure env
execJustProg (line : ls) env =
  case readLine line of
    Left (SyntaxError err) -> print err >> pure env
    Right comm -> case comm of
      Import f ->
        liftIO (readFile (importPath <> f <> ".plam"))
          >>= (\content -> execJustProg (lines content) env)
          >>= execJustProg ls
      Define v e ->
        let (res, env') = evalDefine v e `runState` env
         in case res of
              Left err -> print err >> execJustProg ls env'
              Right f -> execJustProg ls env'
      Evaluate det cbv e -> decideEvaluateProg env det cbv e
      Review r ->
        ( case r of
            "all" -> putStrLn " ENVIRONMENT:" >> mapM_ printGlobal env
            _ -> putStrLn ("--- definition of " <> show r <> ": " <> reviewVariable env r)
        )
          >> execJustProg ls env
      Print s ->
        putStrLn s
          >> execJustProg ls env
      _ -> execJustProg ls env

-------------------------------------------------------------------------------------
isplam :: String -> Bool
isplam s = ".plam" `isSuffixOf` s

-------------------------------------------------------------------------------------
-- MAIN with Read-Evaluate-Print Loop --
-------------------------------------------------------------------------------------
repl :: Environment -> InputT IO ()
repl env =
  getInputLine (boldCyan "pLam>" <> " ")
    >>= ( \case
            Nothing -> pure ()
            Just line
              | line == ":quit" || line == ":q" -> void (outputStrLn (boldCyan "Goodbye!"))
              | otherwise -> execute line env >>= repl
        )

data Options
  = FileInput FilePath
  | Repl Bool

options :: Options.Parser Options
options = fileInput <|> replOpts
  where
    fileInput :: Options.Parser Options
    fileInput = FileInput <$> argument str (metavar "FILE" <> help ".plam file to evaluate")

    replOpts :: Options.Parser Options
    replOpts = Repl <$> switch (long "no-heading" <> help "Don't display the heading")

main :: IO ()
main = do
  args <- execParser (info (options <**> helper) (fullDesc <> progDesc "pure λ-calculus interpreter"))
  case args of
    FileInput file -> readFile file >>= (\content -> execJustProg (lines content) []) >> putStrLn (boldGreen "Done.")
    Repl noHeading ->
      unless noHeading (putStrLn heading)
        >> runInputT defaultSettings {historyFile = Just ".plam-history"} (repl [])
