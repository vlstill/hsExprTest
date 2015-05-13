-- (c) 2012 Martin Jonáš
-- (c) 2014,2015 Vladimír Štill

module Types.Comparing ( expressionsEqual, compareTypes ) where

import Data.List
import Types.Processing
import Types.Formating
import Types.TypeExpression
import Result ( TypingResult ( .. ) )

-- | Compare type context, order does not matter, variable naming does
instance Eq TypeContext where
    (==) (TypeContext a) (TypeContext b) = sort a == sort b

-- | Compare type expressions after normalization (same as 'expressionsEqual')
instance Eq TypeExpression where
    (==) = expressionsEqual

-- | Compare type expressions for equality, normalize them (same as '(==)')
expressionsEqual :: TypeExpression -> TypeExpression -> Bool
expressionsEqual a b = isEqual $ compareTypes a b
  where
    isEqual (TypesEqual _) = True
    isEqual _              = False

-- | Compare type expressions, returning either canonized type, or a textual
-- description of in case of noneqality
compareTypes :: TypeExpression -> TypeExpression -> TypingResult
compareTypes a0 b0 = case (context1 == context2, type1 == type2) of
        (False, True)  -> NotEqual tconmsg
        (False, False) -> NotEqual $ tconmsg ++ " " ++ tmismsg
        (True, False)  -> NotEqual tmismsg
        (True, True)   -> TypesEqual a
  where
    a@(TypeExpression context1 type1) = normalize a0
    b@(TypeExpression context2 type2) = normalize b0

    tconmsg = "Type contex mismatch: " ++ formatContext context1
                            ++ " /= " ++ formatContext context2 ++ "."
    tmismsg = "Type mismatch: " ++ formatType type1 ++ " /= " ++ formatType type2
                            ++ ", could not match " ++ _mismatch type1 type2 ++ "."

_mismatch (TypeApplication t11 t12) (TypeApplication t21 t22) = _mismatch2 t11 t12 t21 t22
_mismatch (FunctionType t11 t12) (FunctionType t21 t22) = _mismatch2 t11 t12 t21 t22
_mismatch (TypeConstructor con1) (TypeConstructor con2) = case con1 == con2 of
    True -> []
    False -> "type constructor " ++ con1 ++ " with " ++ con2
_mismatch (VariableType va1) (VariableType va2) = case va1 == va2 of
    True -> []
    False -> "type variable " ++ va1 ++ " with " ++ va2
_mismatch a@(TupleType tts1) b@(TupleType tts2) = case tts1 == tts2 of
    True -> []
    False -> case length tts1 == length tts2 of
        True -> intercalate ", " $ filter (not . null) $ zipWith _mismatch tts1 tts2
        False -> "tuple type " ++ formatType a ++ " of length " ++ show (length tts1)
            ++ " with tuple type " ++ formatType b ++ " of length " ++ show (length tts2)
_mismatch (ListType t1) (ListType t2) = _mismatch t1 t2
_mismatch a b = _ttype a ++ " " ++ formatType a ++ " with "
                 ++ _ttype b ++ " " ++ formatType b

_mismatch2 t11 t12 t21 t22 = case (t11 == t21, t12 == t22) of
    (True,  True)  -> []
    (False, True)  -> _mismatch t11 t21
    (True,  False) -> _mismatch t12 t22
    (False, False) -> _mismatch t11 t21 ++ ", " ++ _mismatch t12 t22

_ttype (TypeApplication _ _) = "parametrized type"
_ttype (TypeConstructor _)   = "type constructor"
_ttype (FunctionType _ _)    = "function type"
_ttype (VariableType _)      = "type variable"
_ttype (TupleType _)         = "tuple type"
_ttype (ListType _)          = "list type"
