-- @ reflection: True
-- @ require type: True
-- @ allow imports: False

import Test.Expr.Reflection

expr = "lengthFold"

usesFold :: ASTCheck
usesFold studentAST = studentAST `onFunD` "lengthFold" $ callStartsWithOneOfD ['foldr, 'foldl]

lengthFold :: [a] -> Int
lengthFold xs = foldr (\_ l -> 1 + l) 0 xs
