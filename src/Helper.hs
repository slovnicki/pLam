module Helper where

import Control.Monad.State
import Debug.Trace
import Evaluator
import Parser
import Reducer
import Syntax
import System.Console.ANSI.Codes (setSGRCode)
import System.Console.ANSI.Types (Color (..), ColorIntensity (..), ConsoleIntensity (..), ConsoleLayer (..), SGR (..))
import System.Console.Haskeline
import System.IO (Handle, hFlush, hPutStrLn, stdout)

red :: String -> String
red s = setSGRCode [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull Red] <> s <> setSGRCode [Reset]

magenta :: String -> String
magenta s = setSGRCode [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull Magenta] <> s <> setSGRCode [Reset]

cyan :: String -> String
cyan s = setSGRCode [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull Cyan] <> s <> setSGRCode [Reset]

yellow :: String -> String
yellow s = setSGRCode [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull Yellow] <> s <> setSGRCode [Reset]

boldCyan :: String -> String
boldCyan s = setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Cyan] <> s <> setSGRCode [Reset]

boldMagenta :: String -> String
boldMagenta s = setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Magenta] <> s <> setSGRCode [Reset]

boldRed :: String -> String
boldRed s = setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Red] <> s <> setSGRCode [Reset]

boldGreen :: String -> String
boldGreen s = setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Green] <> s <> setSGRCode [Reset]

boldYellow :: String -> String
boldYellow s = setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Yellow] <> s <> setSGRCode [Reset]

-------------------------------------------------------------------------------------
showGlobal :: (String, Expression) -> InputT IO ()
showGlobal (n, e) = outputStrLn ("--- " ++ show n ++ " = " ++ show e)

printGlobal :: (String, Expression) -> IO ()
printGlobal (n, e) = putStrLn ("--- " ++ show n ++ " = " ++ show e)

removeLambda :: String -> String
removeLambda =
  map
    ( \case
        'λ' -> '\\'
        c -> c
    )

saveGlobal :: Handle -> (String, Expression) -> IO ()
saveGlobal h (n, e) = hPutStrLn h (n ++ " = " ++ removeLambda (show e))

convertToName :: Environment -> Expression -> String
convertToName [] exp = findNumeral exp
convertToName ((v, e) : rest) ex
  | alphaEquiv e ex = v
  | otherwise = convertToName rest ex

convertToNames :: Bool -> Bool -> Expression -> Environment -> Expression -> String
convertToNames redexFound redexVarFind redexVar env (Variable v) =
  if redexVarFind && (Variable v == redexVar)
    then red $ show v
    else show v
convertToNames redexFound redexVarFind redexVar env redex@(Application (Abstraction v e) n) =
  if redexFound
    then
      let redex1 = convertToName env redex
       in if redex1 == "none"
            then "(" ++ convertToNames True False redexVar env (Abstraction v e) ++ " " ++ convertToNames True False redexVar env n ++ ")"
            else redex1
    else magenta "(" ++ boldCyan "λ" ++ boldRed (show v) ++ boldCyan "." ++ cyan (" " ++ convertToNames True True (Variable v) env e) ++ boldCyan ") " ++ boldGreen (convertToNames True False redexVar env n) ++ magenta ")"
convertToNames redexFound redexVarFind redexVar env app@(Application m n) =
  let app1 = convertToName env app
   in if app1 == "none"
        then "(" ++ convertToNames redexFound redexVarFind redexVar env m ++ " " ++ convertToNames redexFound redexVarFind redexVar env n ++ ")"
        else app1
convertToNames redexFound redexVarFind redexVar env abs@(Abstraction v e) =
  let abs1 = convertToName env abs
   in if abs1 == "none"
        then "(λ" ++ show v ++ ". " ++ convertToNames redexFound redexVarFind redexVar env e ++ ")"
        else abs1

-- same as convertToNames, but with additional coloring meant for beta nf terms mostly
---------------------------------------------------------------------------------------------------
convertToNamesResult :: Bool -> Bool -> Expression -> Environment -> Expression -> String
convertToNamesResult redexFound redexVarFind redexVar env (Variable v) =
  if redexVarFind && (Variable v == redexVar)
    then red $ show v
    else show v
convertToNamesResult redexFound redexVarFind redexVar env redex@(Application (Abstraction v e) n) =
  if redexFound
    then
      let redex1 = convertToName env redex
       in if redex1 == "none"
            then "(" ++ convertToNamesResult True False redexVar env (Abstraction v e) ++ " " ++ convertToNamesResult True False redexVar env n ++ ")"
            else redex1
    else magenta "(" ++ boldCyan "(λ" ++ boldRed (show v) ++ boldCyan "." ++ cyan (" " ++ convertToNamesResult True True (Variable v) env e) ++ boldCyan ") " ++ boldGreen (convertToNamesResult True False redexVar env n) ++ magenta ")"
convertToNamesResult redexFound redexVarFind redexVar env app@(Application m n) =
  let app1 = convertToName env app
   in if app1 == "none"
        then magenta "(" ++ convertToNamesResult redexFound redexVarFind redexVar env m ++ " " ++ convertToNamesResult redexFound redexVarFind redexVar env n ++ magenta ")"
        else boldGreen app1
convertToNamesResult redexFound redexVarFind redexVar env abs@(Abstraction v e) =
  let abs1 = convertToName env abs
   in if abs1 == "none"
        then cyan "(" ++ boldCyan "λ" ++ show v ++ boldCyan "." ++ " " ++ convertToNamesResult redexFound redexVarFind redexVar env e ++ cyan ")"
        else boldGreen abs1

-----------------------------------------------------------------------------------------------------------
isDefined :: Environment -> String -> Bool
isDefined [] s = False
isDefined ((v, e) : rest) s
  | v == s = True
  | otherwise = isDefined rest s

reviewVariable :: Environment -> String -> String
reviewVariable [] var = "none"
reviewVariable ((v, e) : rest) var
  | v == var = show e
  | otherwise = reviewVariable rest var

-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
-- construct Expression for numeral from num and check equality
---- before was checking alpha equivalence, but we restrict it now
---- numerals will always be with same variables
---- reduces execution time, esspecially for Churchs
findChurch :: Expression -> Int -> String
findChurch exp num
  | exp == createChurch num (Variable (LambdaVar 'x' 0)) = show num
  | num == 199 = "none"
  | otherwise = findChurch exp (num + 1)

findBinary :: Expression -> Int -> String
findBinary exp num
  | exp == fst (betaNF None 0 (createBinary num)) = show num ++ "b"
  | num == 2047 = "none"
  | otherwise = findBinary exp (num + 1)

findNumeral :: Expression -> String
findNumeral abs@(Abstraction (LambdaVar v n) e)
  | v == 'f' = findChurch abs 0
  | v == 'p' = findBinary abs 0
  | otherwise = "none"
findNumeral exp = "none"

-------------------------------------------------------------------------------------
goodCounter :: Int -> Int -> Int
goodCounter num 0 = num
goodCounter _ rednum = rednum

-------------------------------------------------------------------------------------
showResult :: Environment -> EvaluateOption -> Expression -> Int -> String
showResult env evop exp num =
  let expnf = betaNF evop 0 exp
      count = goodCounter num (snd expnf)
   in showSummary env (fst expnf) count

showProgResult :: Environment -> EvaluateOption -> Expression -> Int -> String
showProgResult env evop exp num =
  let expnf = betaNF evop 0 exp
      count = goodCounter num (snd expnf)
   in showSummary env (fst expnf) count

showSummary :: Environment -> Expression -> Int -> String
showSummary env exp count =
  boldGreen "|> " ++ yellow "reductions count               : " ++ boldRed (show count ++ "\n")
    ++ boldGreen "|> "
    ++ yellow "uncurried "
    ++ boldYellow "β-normal"
    ++ yellow " form        : "
    ++ show exp
    ++ "\n"
    ++ boldGreen "|> "
    ++ yellow "curried (partial) "
    ++ boldYellow "α-equivalent"
    ++ yellow " : "
    ++ convertToNamesResult False False (Variable (LambdaVar '.' 0)) env exp

manualReduce :: Environment -> EvaluateOption -> Expression -> Int -> InputT IO ()
manualReduce env evop exp num = do
  outputStrLn (boldMagenta ("#" ++ show num ++ ":") ++ convertToNames False False (Variable (LambdaVar '.' 0)) env exp)
  line <- getInputLine $ boldYellow "Next step?" ++ " [Y/n/f] (f: finish all remaining steps): "
  case line of
    Just "n" ->
      outputStrLn $ showSummary env exp num
    Just "f" -> autoReduce env evop exp num
    _ ->
      if hasBetaRedex exp
        then uncurry (manualReduce env evop) (betaReduction evop num exp)
        else outputStrLn $ showResult env evop exp num

autoReduce :: Environment -> EvaluateOption -> Expression -> Int -> InputT IO ()
autoReduce env evop exp num = do
  outputStrLn (boldMagenta ("#" ++ show num ++ ":") ++ " " ++ convertToNames False False (Variable (LambdaVar '.' 0)) env exp)
  if hasBetaRedex exp
    then uncurry (autoReduce env evop) (betaReduction evop num exp)
    else outputStrLn $ showResult env evop exp num

autoProgReduce :: Environment -> EvaluateOption -> Expression -> Int -> IO ()
autoProgReduce env evop exp num = do
  putStrLn ("#" ++ boldMagenta (show num ++ ":") ++ " " ++ convertToNames False False (Variable (LambdaVar '.' 0)) env exp)
  if hasBetaRedex exp
    then uncurry (autoProgReduce env evop) (betaReduction evop num exp)
    else putStrLn $ showProgResult env evop exp num

-------------------------------------------------------------------------------------

decideEvaluate :: Environment -> EvaluateOption -> EvaluateOption -> Expression -> InputT IO Environment
decideEvaluate env None None e = do
  let (res, env') = evalExp e `runState` env
  outputStrLn $ case res of
    Left err -> show err
    Right exp -> showResult env None exp 0
  return env
decideEvaluate env Detailed None e = do
  let (res, env') = evalExp e `runState` env
  case res of
    Left err -> outputStrLn $ show err
    Right exp -> do
      op <- getInputLine $ boldYellow "Choose stepping option" ++ " ([default] a: auto all, m: manual step-by-step): "
      case op of
        Just "a" -> autoReduce env None exp 0
        Just "m" -> manualReduce env None exp 0
        _ -> autoReduce env None exp 0
  return env
decideEvaluate env None CallByValue e = do
  let (res, env') = evalExp e `runState` env
  outputStrLn $ case res of
    Left err -> show err
    Right exp -> showResult env CallByValue exp 0
  return env
decideEvaluate env Detailed CallByValue e = do
  let (res, env') = evalExp e `runState` env
  case res of
    Left err -> outputStrLn $ show err
    Right exp -> do
      op <- getInputLine $ boldYellow "Choose stepping option" ++ " ([default] a: auto all, m: manual step-by-step): "
      case op of
        Just "a" -> autoReduce env CallByValue exp 0
        Just "m" -> manualReduce env CallByValue exp 0
        _ -> autoReduce env CallByValue exp 0
  return env

decideEvaluateProg :: Environment -> EvaluateOption -> EvaluateOption -> Expression -> IO Environment
decideEvaluateProg env None None e = do
  let (res, env') = evalExp e `runState` env
  putStrLn $ case res of
    Left err -> show err
    Right exp -> showProgResult env None exp 0
  return env
decideEvaluateProg env Detailed None e = do
  let (res, env') = evalExp e `runState` env
  case res of
    Left err -> print err
    Right exp -> autoProgReduce env None exp 0
  return env
decideEvaluateProg env None CallByValue e = do
  let (res, env') = evalExp e `runState` env
  putStrLn $ case res of
    Left err -> show err
    Right exp -> showProgResult env CallByValue exp 0
  return env
decideEvaluateProg env Detailed CallByValue e = do
  let (res, env') = evalExp e `runState` env
  case res of
    Left err -> print err
    Right exp -> autoProgReduce env CallByValue exp 0
  return env
