{-# LANGUAGE LambdaCase, TupleSections #-}
module Types.TH where

import Language.Haskell.TH
import Language.Haskell.TH.ExpandSyns
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

import Types hiding ( Type )

getType :: Name -> ExceptT String Q Type
getType = reify >>> lift >=> unInfo >=> expandSyns >>> lift
  where
    unInfo (ClassI _ _)       = throwE "Unexpected Class in getType"
    unInfo (ClassOpI _ t _ _) = return t
    unInfo (TyConI _)         = throwE "Unexpected TyCon in getType"
    unInfo (FamilyI _ _)      = throwE "Unexpected Family in getType"
    unInfo (PrimTyConI _ _ _) = throwE "Unexpected PrimTyCon in getType"
    unInfo (DataConI _ t _ _) = return t
    unInfo (VarI _ t _ _)     = return t
    unInfo (TyVarI _ _)       = throwE "Unexpected TyVar in getType"

th2ecType :: Type -> ExceptT String Q TypeExpression
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

#if MIN_VERSION_template_haskell(2,10,0)
    getCtx = fmap TypeContext . mapM getCtx1
    getCtx1 x@(AppT _ _) = case c of
                              ConT name -> (,) <$> getName name <*> mapM getType ps
                              _         -> throwE "Expected class name"
      where
        (c:ps) = unapp x
    getCtx1 _ = throwE "Unexpected type in getCtx1"
    unapp (AppT x xs) = x : unapp xs
    unapp x = [x]
                    
#else
    getCtx = fmap TypeContext . mapM (\case
                    ClassP name ts -> (,) <$> getName name <*> mapM getType ts
                    EqualP _ _     -> throwE "unexpected EqualP in context")
#endif
    getName = return . show
    getLit (NumTyLit x) = return $ show x
    getLit (StrTyLit x) = return $ show x
    
    
