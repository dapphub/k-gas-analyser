module Solver where

import Gas

-- percolates ITEs to the top
normalise :: GasExpr -> GasExpr
normalise (Nullary n) = Nullary n
normalise (Unary op (ITE c e f)) = normalise (ITE c ope opf)
  where ope = Unary op e
        opf = Unary op f
normalise (Unary op e) = normalise (Unary op e')
  where e' = normalise e
normalise (Binary op (ITE c e f) g) = normalise (ITE c eg fg)
  where eg = Binary op e g
        fg = Binary op f g
normalise (Binary op f (ITE c e g)) = normalise (ITE c fe fg)        
  where fe = Binary op f e
        fg = Binary op f g
normalise (Binary op f g) = normalise (Binary op f' g')
  where f' = normalise f
        g' = normalise g
normalise (ITE c f g) = ITE c f' g'
  where f' = normalise f
        g' = normalise g

solveLeaves :: GasExpr -> GasExpr
solveLeaves = _
