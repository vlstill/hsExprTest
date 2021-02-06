{-# LANGUAGE CPP #-}

module Test.Expr.Internal.Compat where

import Language.Haskell.TH ( Exp )

#if MIN_VERSION_template_haskell(2,16,0)
wrapTupElemE :: Exp -> Maybe Exp
wrapTupElemE = Just
#else
wrapTupElemE :: Exp -> Exp
wrapTupElemE = id
#endif
