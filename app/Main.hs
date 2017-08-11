module Main where

import Syntax
import Parser


main :: IO ()
main = do
        putStrLn "type expression: "
        line <- getLine
        let expr = parseExpr line
        case expr of
            Left err -> print err
            Right ex -> print ex
        main
