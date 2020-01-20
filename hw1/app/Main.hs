module Main where

import           Lexer     (alexScanTokens)
import           Parser    (parseLambda)
import           ParseTree
import           System.IO (isEOF)

getString :: IO String
getString = do
    endMark <- isEOF
    if endMark then return ""
    else do
         current <- getLine
         tail <- getString
         return $! current ++ " " ++ tail


main :: IO ()
main = do
    line <- getString
    case parseLambda $ alexScanTokens line of
      Left err   -> putStrLn err
      Right expr -> putStrLn $ show expr
