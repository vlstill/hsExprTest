module Test.TH.Utils where

import Language.Haskell.TH ( Exp (..), Lit (..), Name, Ppr, pprint, Q, reportWarning )

-- | Create a function application expression calling given function with given arguments.
apply :: Name -> [Exp] -> Exp
apply name exs = apply' (VarE name) exs

-- | Like 'apply', but function is given as an expression.
apply' :: Exp -> [Exp] -> Exp
apply' fun exs = foldl AppE fun exs

_dbg :: Ppr a => Q a -> Q a
_dbg qx = qx >>= \x -> reportWarning (pprint x) >> pure x

-- | Show string representation of template haskell expression, such as AST:
-- @$(showQ [| \x -> x |])@
--
-- This is useful to see how the code is represented
showQ :: Show a => Q a -> Q Exp
showQ x = LitE . StringL . show <$> x

-- | Show string representation of template haskell expression without 'Q' Monad, such as names:
-- @$(showQ' 'id)@
--
-- This is useful to see how the code is represented
showQ' :: Show a => a -> Q Exp
showQ' = pure . LitE . StringL . show
