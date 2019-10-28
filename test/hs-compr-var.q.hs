-- @ reflection: True
-- @ require type: True

import Test.Expr.Reflection

expr = "empty"

allUseComprehention :: ASTCheck
allUseComprehention studentAST = mapM_ isComprehentionD studentAST

empty :: [a]
empty = [ undefined | _ <- [] ]
