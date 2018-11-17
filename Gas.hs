module Gas where

-- grammar for K gas expressions
data GasExpr = Nullary NullOp
  | Unary UnOp GasExpr
  | Binary BinOp GasExpr GasExpr
  | ITE Cond GasExpr GasExpr
  deriving Show

data NullOp = StartGas | Literal Int
  deriving Show
data UnOp = SixtyFourth
  deriving Show
data BinOp = Add | Sub
  deriving Show
data Cond = Cond String
  deriving Show

bracket :: String -> String
bracket s = "( " ++ s ++ " )"

unparse :: GasExpr -> String
unparse (Nullary StartGas) = "VGas"
unparse (Nullary (Literal x)) = show x
unparse (Unary SixtyFourth e) = ((bracket . unparse) e) ++ " / 64"
unparse (Binary op e f) = bracket (s ++ opstr ++ t)
  where s = unparse e
        t = unparse f
        opstr = case op of
                  Add -> " + "
                  Sub -> " - "
unparse (ITE (Cond c) e f) = "#if "
  ++ c ++ " #then "
  ++ s ++ " #else " ++ t
  ++ " #fi"
  where s = unparse e
        t = unparse f
