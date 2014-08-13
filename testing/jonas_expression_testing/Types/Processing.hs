module Types.Processing (normalize, getTestableArguments) where

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
-- | It substitutes all type variables with t1 ... tn in the order of the first occurance.
normalize :: TypeExpression -> TypeExpression
normalize (TypeExpression context inputType) = TypeExpression (normalizeContext substitution context) (normalizeType substitution inputType)
	where substitution = Map.fromList (getNormalizingSubstitution inputType)

-- | Function normalizeContext normalizes type context of the type expression.
normalizeContext :: Map.Map TypeVariable Int -> TypeContext -> TypeContext
normalizeContext substitution (TypeContext items) = TypeContext (map (\(t, v) -> (t, substitute substitution v)) items)
	
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
substitute substitution variable = case (Map.lookup variable substitution) of 
	Nothing -> error "Variable not found"
	Just newVar -> 't' : show newVar

-- | Function getTestableArguments returns number of function simple arguments in the given type expression or Nothing if on of the arguments is another function.
getTestableArguments :: TypeExpression -> Maybe Int
getTestableArguments (TypeExpression _ inputType) = fmap ((+)(-1)) (getTestableArguments' inputType)

-- | Function getTestableArguments' returns number of function simple arguments in the given type without type context or Nothing if on of the arguments is another function.
getTestableArguments' :: Type -> Maybe Int
getTestableArguments' (TypeApplication type1 type2) = if (isSimple type1 && isSimple type2) then Just 1 else Nothing
getTestableArguments' (TypeConstructor type1) = Just 1
getTestableArguments' (FunctionType type1 type2) = if (isSimple type1) then (fmap (+1) (getTestableArguments' type2)) else Nothing 
getTestableArguments' (VariableType var) = Just 1
getTestableArguments' (TupleType types) = if (all (isJust . getTestableArguments') types) then Just 1 else Nothing
getTestableArguments' (ListType listT) = if (isJust (getTestableArguments' listT)) then Just 1 else Nothing

-- | Function isSimple returns boolean indicating wheather given type contains another functions as arguments, list elements or tuple elements.
-- | Functions which are not simple cannot be tested in the current version.
isSimple :: Type -> Bool
isSimple (TypeApplication type1 type2) = isSimple type1 && isSimple type2
isSimple (TypeConstructor type1) = True
isSimple (FunctionType type1 type2) = False
isSimple (VariableType var) = True
isSimple (TupleType types) = all isSimple types
isSimple (ListType listT) = isSimple listT