module Main where

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Aeson                (decode)
import System.Environment        (getArgs)
import System.Exit               (exitWith,
                                  ExitCode(ExitSuccess),
                                  ExitCode(ExitFailure))

import Solver    (solve)
import Gas       (unparse)
import Parser    (readGasExpr)
import KastParse (Kast, kastToGasExpr)

main :: IO ()
main = do
  x <- parseArgs =<< getArgs
  case gasASTSolve x of
    Left err -> (putStrLn $ "Failed in parsing AST: " ++ err) >> die
    Right solution -> (putStr solution) >> exit

parseArgs :: [String] -> IO String
parseArgs ["--help"] = usage   >> exit
parseArgs []         = getContents
parseArgs (fs:_)     = readFile fs

usage   = putStrLn "Usage: tac [--help] [file ..]"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

gasASTSolve :: String -> Either String String
gasASTSolve s = let Just gaskast = (decode (fromString s)) :: Maybe Kast
                    e = kastToGasExpr gaskast in
                  case e of
                    Left error -> Left error
                    Right res  -> Right $ unparse $ solve $ res
