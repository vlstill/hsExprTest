-- @ reflection: True
-- @ require type: True

import Test.Expr.Reflection
import Data.Maybe

expr = "empty"

allUseComprehention :: ASTCheck
allUseComprehention studentAST = mapM_ isComprehentionD studentAST

empty :: [a]
empty = [ undefined | _ <- [] ]
