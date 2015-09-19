{-# LANGUAGE LambdaCase, TupleSections #-}
module Types.TH where

import Language.Haskell.TH
import Language.Haskell.TH.ExpandSyns
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Types hiding ( Type )


import qualified Test.QuickCheck.Range as QCR

thTypeOf :: Name -> Q Type
thTypeOf = reify >=> unInfo >=> expandSyns
  where
    unInfo (ClassI _ _)       = error "Unexpected Class in thTypeOf"
    unInfo (ClassOpI _ t _ _) = return t
    unInfo (TyConI _)         = error "Unexpected TyCon in thTypeOf"
    unInfo (FamilyI _ _)      = error "Unexpected Family in thTypeOf"
    unInfo (PrimTyConI _ _ _) = error "Unexpected PrimTyCon in thTypeOf"
    unInfo (DataConI _ t _ _) = return t
    unInfo (VarI _ t _ _)     = return t
    unInfo (TyVarI _ _)       = error "Unexpected TyVar in thTypeOf"

th2ecType :: Type -> Q TypeExpression
th2ecType typ = case typ of
    (ForallT _ ctx typ) -> TypeExpression <$> getCtx ctx <*> getType typ
    _                   -> TypeExpression (TypeContext []) <$> getType typ
  where
    getType (AppT t1 t2) = TypeApplication <$> getType t1 <*> getType t2
    getType (VarT name)  = TypeVariable <$> getName name
    getType (ConT name)  = TypeConstructor . TyCon <$> getName name
    getType (TupleT n)   = return $ TypeConstructor (TupleTyCon n)
    getType ArrowT       = return $ TypeConstructor FunTyCon
    getType ListT        = return $ TypeConstructor ListTyCon
    getType (LitT lit)   = TypeConstructor . TyLit <$> getLit lit
    getType (SigT t k)   = getType t
    getType PromotedConsT = return $ TypeConstructor (TyLit "(':)")
    getType PromotedNilT  = return $ TypeConstructor (TyLit "'[]")
    getType x            = error $ show x

#if MIN_VERSION_template_haskell(2,10,0)
    getCtx = fmap TypeContext . mapM getCtx1
    getCtx1 x@(AppT _ _) = case c of
                              ConT name -> (,) <$> getName name <*> mapM getType ps
                              _         -> error "Expected class name"
      where
        (c:ps) = unapp x
    getCtx1 _ = error "Unexpected type in getCtx1"
    unapp (AppT x xs) = x : unapp xs
    unapp x = [x]
                    
#else
    getCtx = fmap TypeContext . mapM (\case
                    ClassP name ts -> (,) <$> getName name <*> mapM getType ts
                    EqualP _ _     -> error "unexpected EqualP in context")
#endif
    getName = return . show
    getLit (NumTyLit x) = return $ show x
    getLit (StrTyLit x) = return $ show x
    
    
{-

foo :: QCR.Range Int 0 1
foo = undefined

bar :: QCR.Ranges Int [(0, 1), (3,4)]
bar = undefined

-}
