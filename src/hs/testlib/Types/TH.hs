{-# LANGUAGE LambdaCase, TupleSections #-}

-- | Support for getting type with expanded type synonyms using Template Haskell
--
-- (c) 2015 Vladimír Štill

module Types.TH where

import Language.Haskell.TH
import Language.Haskell.TH.ExpandSyns
import Control.Monad
import Control.Arrow
import Data.List
import Data.Function
import Language.Haskell.Interpreter ( interpret, as, MonadInterpreter )

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Types hiding ( Type )

-- | get normalize (with expanded synonyms) 'TypeExpression' from function name
typeOf'' :: Name -> Q TypeExpression
typeOf'' = thTypeOf >=> th2ecType >=> simplifyNames >>> fmap simplifyVars

-- | get normalize (with expanded synonyms) 'TypeExpression' from interpreted function name
typeOf' :: (Functor m, MonadInterpreter m) => String -> m TypeExpression
typeOf' fun = read <$> interpret ("$(stringE . show =<< typeOf'' '" ++ fun ++")") (as :: String)

-- | get Template Haskell's type ('Language.Haskell.TH.Type') of function
thTypeOf :: Name -> Q Type
thTypeOf = reify >=> unInfo >=> expandSyns
  where
    unInfo (ClassI {})        = error "Unexpected Class in thTypeOf"
    unInfo (ClassOpI _ t _ _) = return t
    unInfo (TyConI {})        = error "Unexpected TyCon in thTypeOf"
    unInfo (FamilyI {})       = error "Unexpected Family in thTypeOf"
    unInfo (PrimTyConI {})    = error "Unexpected PrimTyCon in thTypeOf"
    unInfo (DataConI _ t _ _) = return t
    unInfo (VarI _ t _ _)     = return t
    unInfo (TyVarI {})        = error "Unexpected TyVar in thTypeOf"

-- | Convert Template Haskell's type to hsExprTest's 'TypeExpression'
th2ecType :: Type -> Q TypeExpression
th2ecType = \case
    (ForallT _ ctx typ) -> TypeExpression <$> getCtx ctx <*> get typ
    typ                 -> TypeExpression (TypeContext []) <$> get typ
  where
    get (AppT t1 t2)       = TypeApplication <$> get t1 <*> get t2
    get (VarT name)        = TypeVariable <$> getName name
    get (ConT name)        = TypeConstructor . TyCon <$> getName name
    get (TupleT n)         = return $ TypeConstructor (TupleTyCon n)
    get ArrowT             = return $ TypeConstructor FunTyCon
    get ListT              = return $ TypeConstructor ListTyCon
    get (LitT lit)         = TypeConstructor . TyLit <$> getLit lit
    get (SigT t _)         = get t
    get PromotedConsT      = return $ TypeConstructor (TyCon "'(:)")
    get PromotedNilT       = return $ TypeConstructor (TyCon "'[]")
    get (PromotedTupleT n) = return $ TypeConstructor (TyCon $ "'(" ++ replicate n ',' ++ ")")
    get (PromotedT n)      = TypeConstructor . TyCon . ('\'':) <$> getName n
    get (ForallT _ [] typ) = get typ
    get (ForallT {})       = error "th2ecType: RankNTypes not supported"
    get (UnboxedTupleT _)  = error "th2ecType: Unboxed tuples not supported"
#if MIN_VERSION_template_haskell(2,10,0)
    get EqualityT          = return . TypeConstructor $ TyCon "(~)"
#endif
    get StarT              = error "th2ecType: Unexpected kind signature (*)"
    get ConstraintT        = error "th2ecType: Unexpected kind signature (Constraint)"

#if MIN_VERSION_template_haskell(2,10,0)
    getCtx = fmap TypeContext . mapM getCtx1
    getCtx1 x@(AppT _ _) = case c of
                              ConT name -> (,) <$> getName name <*> mapM get ps
                              _         -> error "Expected class name"
      where
        (c:ps) = unapp x
    getCtx1 _ = error "Unexpected type in getCtx1"
    unapp (AppT x xs) = x : unapp xs
    unapp x = [x]
                    
#else
    getCtx = fmap TypeContext . mapM (\case
                    ClassP name ts -> (,) <$> getName name <*> mapM get ts
                    EqualP _ _     -> error "unexpected EqualP in context")
#endif
    getName = return . show
    getLit (NumTyLit x) = return $ show x
    getLit (StrTyLit x) = return $ show x

-- | Try to simplify names (i.e. GHC.Read.Read ~> Read, GHC.Types.Int ~> Int)
simplifyNames :: TypeExpression -> Q TypeExpression
simplifyNames (TypeExpression (TypeContext ctx) typ) =
    TypeExpression <$> (TypeContext <$> mapM (\(x,y) -> (,) <$> simplifyName x <*> mapM simplifyTy y) ctx)
                   <*> simplifyTy typ
  where
    simplifyTy = foldType (liftM2 TypeApplication) (\case
                    TyCon name -> TypeConstructor . TyCon <$> simplifyName name
                    x          -> pure $ TypeConstructor x
                  ) (pure . TypeVariable)
    simplifyName name = lookupTypeName suf >>= \case
                        Just n | name == show n -> return suf
                        _                       -> return name
      where
        suf = reverse . takeWhile (/= '.') . reverse $ name

-- | Safely simplify type variable names (i.e. a_1234 -> a_1234 ~> a -> a, a_1234 -> a_1235 ~> a1 -> a2)
simplifyVars :: TypeExpression -> TypeExpression
simplifyVars expr = expr // subst
  where
    vars = groupBy ((==) `on` strip) . sort $ typeVars expr
    strip = takeWhile (/= '_')
    subst = concatMap mksub vars
    mksub [x] = [(x, TypeVariable (strip x))]
    mksub xs  = map (\(x, i) -> (x, TypeVariable (strip x ++ show i))) $ zip xs [1..]

