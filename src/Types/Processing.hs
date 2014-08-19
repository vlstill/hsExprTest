{-# LANGUAGE TupleSections #-}

-- (c) 2012 Martin Jonáš
-- (c) 2014 Vladimír Štill

module Types.Processing ( normalize ) where

import Types.TypeExpression
import Data.List
import Data.Maybe
import qualified Data.Map as Map

-- | Function getTypeVariables returns type variables of given type expressions in the order of the first occurance.
getTypeVariables :: Type -> [TypeVariable]
getTypeVariables (TypeApplication type1 type2) = getTypeVariables type1 ++ getTypeVariables type2
getTypeVariables (TypeConstructor type1) = []
getTypeVariables (FunctionType type1 type2) = getTypeVariables type1 ++ getTypeVariables type2
getTypeVariables (VariableType var) = [var]
getTypeVariables (TupleType types) = concatMap getTypeVariables types
getTypeVariables (ListType listType) = getTypeVariables listType

-- | -- | Function getTypeVariables returns type variables of given type expressions in the order of the first occurance and numbers them with natural numbers in this order.
getNormalizingSubstitution :: Type -> [(TypeVariable, Int)]
getNormalizingSubstitution = flip zip [1..] . nub . getTypeVariables

-- | Function normalize normalizes given type expression.
-- | It substitutes all type variables with t1 ... tn in the order of the first
-- occurance and removes duplicate and nonused constexts.
normalize :: TypeExpression -> TypeExpression
normalize (TypeExpression context inputType) = TypeExpression (normalizeContext substitution context) (normalizeType substitution inputType)
    where substitution = Map.fromList (getNormalizingSubstitution inputType)

-- | Function normalizeContext normalizes type context of the type expression.
-- this will: - apply substitution
--            - remove any nonused contexts
--            - remove any duplicated contexts
normalizeContext :: Map.Map TypeVariable Int -> TypeContext -> TypeContext
normalizeContext substitution (TypeContext items) =
    TypeContext . nub $ (mapMaybe (\(t, v) -> substitute' substitution v >>= return . (t, )) items)

-- | Function normalizeContext normalizes type without type context.
normalizeType :: Map.Map TypeVariable Int -> Type -> Type
normalizeType substitution (TypeApplication type1 type2) = TypeApplication (normalizeType substitution type1) (normalizeType substitution type2)
normalizeType substitution (TypeConstructor type1) = TypeConstructor type1
normalizeType substitution (FunctionType type1 type2) = FunctionType (normalizeType substitution type1) (normalizeType substitution type2)
normalizeType substitution (VariableType var) = VariableType (substitute substitution var)
normalizeType substitution (TupleType types) = TupleType $ map (normalizeType substitution) types
normalizeType substitution (ListType listT) = ListType $ normalizeType substitution listT

-- | Function substitute substitutes type variable with the associated new name in given Map and reutrns the new name.
substitute :: Map.Map TypeVariable Int -> TypeVariable -> TypeVariable
substitute substitution variable =
    case substitute' substitution variable of
        Just ok -> ok
        Nothing -> error "Variable not found"

substitute' :: Map.Map TypeVariable Int -> TypeVariable -> Maybe TypeVariable
substitute' substitution variable = Map.lookup variable substitution >>= \newVar ->
                                    Just $ 't' : show newVar
