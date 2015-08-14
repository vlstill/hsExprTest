{-# LANGUAGE DeriveDataTypeable, TupleSections #-}

-- (c) 2012 Martin Jonáš
-- (c) 2015 Vladimír Štill

module Types.TypeExpression (
      -- * Type representation
      TypeExpression(..)
    , TypeContext(..)
    , Type(..)
    , TypeClass
    , TypeVar
    , TypeConstr(..)
    -- * katamorphisms and quieriing of types
    , foldType
    , isPolymorphic
    , CType(..)
    -- * substitution
    , Substitution
    , (//)
    -- * functions for construction and destruction of types
    , splitFunApp
    , tupleType
    , (-->)
    -- * Unification
    , unifyTypes
    ) where

import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.List
import Data.Data ( Data )
import Data.Typeable ( Typeable )
import Data.Function ( on )

-- | Data type TypeExpression represents parsed type expression
data TypeExpression = TypeExpression TypeContext Type
    deriving ( Show, Typeable, Data )

-- | Data type TypeContext represents parsed type context
data TypeContext = TypeContext [(TypeClass, [Type])]
    deriving ( Show, Typeable, Data )

-- | Data type TypeExpression represents parsed type witnout the type context
data Type = TypeApplication Type Type -- ^ Application of the (partially applied)
            -- type constructor or variable on another type
          | TypeConstructor TypeConstr
          | TypeVariable TypeVar
          deriving ( Show, Eq, Ord, Typeable, Data )

type TypeClass = String

type TypeVar = String

-- | Type costructors: build-in type constructors ->, [], (,), (,,)… are handled
-- separatelly to allow for better analysis of those cases
data TypeConstr = FunTyCon
                | ListTyCon
                | TupleTyCon Int -- ^ tuple with arity
                | TyCon String -- ^ any other type constructor
                deriving ( Show, Eq, Ord, Typeable, Data )

foldType :: (a -> a -> a) -- ^ TypeApplication
         -> (TypeConstr -> a) -- ^ TypeConstructor
         -> (TypeVar -> a) -- TypeVariable
         -> Type -> a
foldType tyApp tyCon tyVar = ftgo
  where
    ftgo (TypeApplication t1 t2) = ftgo t1 `tyApp` ftgo t2
    ftgo (TypeConstructor t)     = tyCon t
    ftgo (TypeVariable v)        = tyVar v

isPolymorphic :: Type -> Bool
isPolymorphic = foldType (||) (const False) (const True)

type Substitution = [(TypeVar, Type)]

class CType t where
    substitute :: Substitution -> t -> t
    -- | Type variables in order of first occurrence in type (first in type and
    -- then in context for TypeExpression)
    typeVars :: t -> [TypeVar]
    -- | Type is plain if all type context are of form Class tyVar1 … tyVarN
    plainType :: t -> Bool

-- | infix operator equivalent to @'flip' 'substitute'@
(//) :: CType t => t -> Substitution -> t
(//) = flip substitute

fullyInstantiated :: CType t => t -> Bool
fullyInstantiated = null . typeVars

instance CType Type where
    substitute sub = sgo
      where
        sgo = foldType (TypeApplication `on` sgo) TypeConstructor subst
        subst :: TypeVar -> Type
        subst x = fromMaybe (TypeVariable x) (x `lookup` sub)
    typeVars = nub . foldType (++) (const []) (:[])
    plainType _ = True

instance CType TypeContext where
    substitute sub (TypeContext con) = TypeContext scon
      where
        scon = mapMaybe (second (map (substitute sub)) >>> maybeDrop) con
        maybeDrop (c, t)
          | all fullyInstantiated t = Nothing
          | otherwise               = Just (c, t)
    typeVars (TypeContext con) = nub $ concatMap (concatMap typeVars . snd) con
    plainType (TypeContext con) = all (all (foldType (\_ _ -> False) (const False) (const True)) . snd) con

instance CType TypeExpression where
    substitute sub (TypeExpression con ty) = TypeExpression (substitute sub con) (substitute sub ty)

    -- note: order is important here: in normalization, we reauire that variables are
    -- sorted by first occurence in type, not in context as that can have arbitrary
    -- order
    typeVars (TypeExpression con ty) = nub $ typeVars ty ++ typeVars con
    plainType (TypeExpression con ty) = plainType con && plainType ty

-- | split type application, for type of form @a -> b@ returns @Just (a, b)@, othrerwise 'Nothing'
splitFunApp :: Type -> Maybe (Type, Type)
splitFunApp ((TypeConstructor FunTyCon `TypeApplication` arg1) `TypeApplication` arg2) = Just (arg1, arg2)
splitFunApp _ = Nothing

-- | Function arrow operator of symbolic types
(-->) :: Type -> Type -> Type
a --> b = (TypeConstructor FunTyCon `TypeApplication` a) `TypeApplication` b
infixl -->

-- | Return tuple type for given parameters
tupleType :: [Type] -> Type
tupleType args = foldl TypeApplication (TypeConstructor (TupleTyCon n)) args
  where
    n = length args

type Unification = (Substitution, Substitution)

-- | Unify two types, returns most general unification
unifyTypes :: Type -> Type -> Either String Unification
unifyTypes ta tb = (,) <$> ((`composeSubst` tas) <$> mgu) <*> ((`composeSubst` tbs) <$> mgu)
  where
    mgu :: Either String Substitution
    mgu = ((ta // tas) `unsafeUnify` (tb // tbs)) >>= substitutionClosure
    tas = addPrefixSubst "a_" ta
    tbs = addPrefixSubst "b_" tb

substitutionClosure :: Substitution -> Either String Substitution
substitutionClosure = sort >>> nub >>> groupBy ((==) `on` fst) >>> mapM merge >=> clo
  where
    merge :: Substitution -> Either String (TypeVar, Type)
    merge ((va, ta):(vb, tb):xs) = (va, ) . (ta //) <$> ((ta `unsafeUnify` tb) >>= substitutionClosure)
    merge [x] = Right x

    clo :: Substitution -> Either String Substitution
    clo sub0 = mapM (\(v, t) -> (v, ) <$> unroll [v] t) sub
      where
        -- first we have to canonize equality chains, therefore we calculate
        -- equality clasees and then change any variable-to-variable substitution
        -- so that it substitutes to lexicographically first variable in each
        -- of the classes
        vars = map fst sub0
        triv = ($ sub0) $
               filter (\(v, t) -> case t of TypeVariable _ -> True; _ -> False) >>>
               map (\(v, TypeVariable t) -> (v, t))
        classes = ($ vars) $ map (:[]) >>> cloop >>> map (filter (`elem` vars))
        cloop vars0 = let vars1 = ($ vars0) $
                                  map (concatMap (\v -> v : map snd (filter ((== v) . fst) triv))) >>>
                                  map (sort >>> nub) >>> sort >>> nub
                      in if vars1 == vars0 then vars0
                                           else cloop vars1
        sub = ($ sub0) $ map standardize >>> filter (\(t, v) -> case v of TypeVariable tt -> t /= tt; _ -> True)
        standardize (v, TypeVariable v1) = (v, TypeVariable . head . fromJust $ find (v `elem`) classes)
        standardize x = x

        -- when we have such a processes substitution, we can unroll it fully
        -- and perform occurs check
        unroll :: [TypeVar] -> Type -> Either String Type
        unroll seen@(v:_) = foldType (liftM2 TypeApplication) (return . TypeConstructor) unrollTyVar >=> \tt ->
                            case tt of
                                TypeVariable _ -> Right tt
                                _ | tt `contains` v ->
                                        Left $ "Unification error: occurs check `" ++ v ++
                                        "' in `" ++ show tt ++ "'"
                                  | otherwise -> Right tt
          where
            unrollTyVar :: TypeVar -> Either String Type
            unrollTyVar var
              | var `elem` seen          = Right $ TypeVariable var
              | Just t <- lookup var sub = unroll (var:seen) t
              | otherwise = Right $ TypeVariable var
    contains ty var = foldType (||) (const False) (== var) ty

-- | add specified prefix to all type variables in type
addPrefixSubst :: CType t => String -> t -> Substitution
addPrefixSubst prefix = typeVars >>> map (id &&& ((prefix ++) >>> TypeVariable))

composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s2 = map (second (// s2))

unsafeUnify :: Type -> Type -> Either String Substitution
unsafeUnify (TypeApplication ta1 ta2) (TypeApplication tb1 tb2) =
    (++) <$> (ta1 `unsafeUnify` tb1) <*> (ta2 `unsafeUnify` tb2)
unsafeUnify (TypeConstructor ca) (TypeConstructor cb)
    | ca == cb  = Right []
    | otherwise = Left $ "Unification error: type constructor mismatch (" ++
                  "could not match `" ++ show ca ++ "' with `" ++ show cb ++ "')"
-- avoid breaking symmetry of equality of type variables
unsafeUnify tva@(TypeVariable va) tvb@(TypeVariable vb) = Right [(va, tvb), (vb, tva)]
unsafeUnify (TypeVariable var) any = Right [(var, any)]
unsafeUnify any (TypeVariable var) = Right [(var, any)]
unsafeUnify ta tb = Left $ "Unification error: could not match `" ++ show ta ++
                           "' with `" ++ show tb ++ "'"
