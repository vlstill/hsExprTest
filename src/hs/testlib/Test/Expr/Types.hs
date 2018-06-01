{-# LANGUAGE TemplateHaskell #-}

module Test.Expr.Types where

import Language.Haskell.TH ( Q, Type (..), Name, reifyInstances, TyVarBndr (..), Cxt )

-- | Get arity of a type (i.e. the number of arguments needed to make the value
-- fully applied).
--
-- $('showQ' $ arity <$> [t| forall a . a -> (a -> a) -> (Int -> Int) |])@
arity :: Type -> Int
arity t = length (fst (uncurryType t))

-- | Get a list of types of all arguments and the return type. All context are
-- stripped.
uncurryType :: Type -> ([Type], Type)
uncurryType t0 = let t = unct t0 in (init t, last t)
  where
    unct (AppT (AppT ArrowT t1) t2) = t1 : unct t2
    unct (ForallT _ _ ty) = unct ty
    unct x = [x]

-- | Is the top-level type constructor a fully applied (->)?
isFunctionType :: Type -> Bool
isFunctionType (AppT (AppT ArrowT _) _) = True
isFunctionType _                        = False

-- | Does given type have instance of given class?
hasInstance :: Type -> Name -> Q Bool
hasInstance t cls = (== 1) . length <$> reifyInstances cls [t]

-- | Merge type contexts which can be merged. Note: this functions works
-- correctly only if none of the quantifiers shadows any type variable.
-- This seems to be the case both for reified types (where multiple contexts
-- occur for class methods) and for @[t| … |]@ expressions.
normalizeContext :: Type -> Type
normalizeContext = set . nc
  where
    set :: ([TyVarBndr], Cxt, Type) -> Type
    set ([], [], t)    = t
    set (bndr, cxt, t) = ForallT bndr cxt t

    nc :: Type -> ([TyVarBndr], Cxt, Type)
    nc (ForallT bndr cxt t0) = (bndr ++ bndrs, cxt ++ cxts, t)
      where
        (bndrs, cxts, t) = nc t0
    nc (AppT (AppT ArrowT l) r) = (bndrs, cxts, AppT (AppT ArrowT nl) nr)
      where
        nl = normalizeContext l
        (bndrs, cxts, nr) = nc r
    nc t = ([], [], t)
