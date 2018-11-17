module Main where
import Solver (solveLeaves)
import Gas (unparse)
import Parser (readGasExpr)

main :: IO ()
main = interact gasSolverInteraction

gasSolverInteraction :: String -> String
gasSolverInteraction = unparse . solveLeaves . readGasExpr
