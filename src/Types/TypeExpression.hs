{-# LANGUAGE DeriveDataTypeable, TupleSections #-}

-- (c) 2012 Martin Jonáš
-- (c) 2015 Vladimír Štill

module Types.TypeExpression 
    ( TypeExpression(..)
    , TypeContext(..)
    , Type(..)
    , TypeClass
    , TypeVar
    , TypeConstr(..)
    , foldType
    , isPolymorphic
    , CType(..)
    , Substitution
    , (//)
    -- * functions for construction and destruction of types
    , splitFunApp
    , tupleType
    , (-->)
    ) where

import Control.Arrow
import Data.Maybe
import Data.List
import Data.Data ( Data )
import Data.Typeable ( Typeable )

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
foldType tyApp tyCon tyVar typ = ftgo typ
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
    substitute sub t = sgo t
      where
        sgo = foldType (\t1 t2 -> TypeApplication (sgo t1) (sgo t2)) TypeConstructor subst
        subst :: TypeVar -> Type
        subst x = case x `lookup` sub of
                    Just ty -> ty
                    Nothing -> TypeVariable x
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
