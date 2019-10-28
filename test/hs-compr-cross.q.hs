-- @ reflection: True
-- @ require type: True
-- @ allow imports: False

import Test.Expr.Reflection

expr = "cross"

allUseComprehention :: ASTCheck
allUseComprehention studentAST = mapM_ isComprehentionD studentAST

cross :: Int -> Int -> [(Int, Int)]
cross ma mb = [ (a, b) | a <- [0..ma], b <- [0..mb] ]
