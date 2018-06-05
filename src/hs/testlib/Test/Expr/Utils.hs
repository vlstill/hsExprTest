{-# LANGUAGE TemplateHaskell #-}

-- | Miscellaneous utilities for Template Haskell and other.
--
-- (c) 2018 Vladimír Štill

module Test.Expr.Utils ( -- * Template Haskell Utilities
                         apply, apply', dbg_, showQ, showQ'
                       , -- * Other Utilities
                         pfail
                       ) where

import Control.Monad.Fail ( fail )
import Prelude hiding ( fail )
import Language.Haskell.TH ( Exp (..), Lit (..), Name, Ppr, pprint, Q, reportWarning )
import Text.Printf.Mauke.TH ( sprintf )

-- | Create a function application expression calling given function with given arguments.
apply :: Name -> [Exp] -> Exp
apply name exs = apply' (VarE name) exs

-- | Like 'apply', but function is given as an expression.
apply' :: Exp -> [Exp] -> Exp
apply' fun exs = foldl AppE fun exs

dbg_ :: Ppr a => Q a -> Q a
dbg_ qx = qx >>= \x -> reportWarning (pprint x) >> pure x

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

pfail :: String -> Q Exp
pfail str = pushFail <$> sprintf str
  where
    pushFail (LamE bnd body) = LamE bnd (pushFail body)
    pushFail body            = VarE 'fail `AppE` body
