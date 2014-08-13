module Types.Comparing where

import Data.List
import Types.TypeExpression
import Types.Processing

instance Eq TypeContext where
	(==) (TypeContext a) (TypeContext b) = sort a == sort b
	
instance Eq TypeExpression where
	(==) = expressionsEqual
	
expressionsEqual :: TypeExpression -> TypeExpression -> Bool
expressionsEqual a b = context1 == context2 && type1 == type2
	where 	
		(TypeExpression context1 type1) = normalize a
		(TypeExpression context2 type2) = normalize b