module Main where
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)

import Solver (solve)
import Gas (unparse)
import Parser (readGasExpr)

example = "((VGas - 1257) - ( 700 + ( VGas - 1957 - ( VGas - 1957 ) / 64 ) ) + #if notBool ABI_x == 0 #then ((VGas - 1957) - ( VGas - 1957 ) / 64) - 20491 #else ((VGas - 1957)  - ( VGas - 1957 ) / 64) - 5491 #fi) - 41"

main :: IO ()
main = interactErrorful gasSolverInteraction

interactErrorful :: (String -> Either String String) -> IO ()
interactErrorful i = do
  s <- getContents
  let result = i s
  let exitOutput = case result of
                     Left error -> hPutStrLn stderr error >> exitWith (ExitFailure 1)
                     Right output -> putStr output
  exitOutput

gasSolverInteraction :: String -> Either String String
gasSolverInteraction s = case readGasExpr s of
                           Left error -> Left error
                           Right result -> Right ((unparse . solve) result)
