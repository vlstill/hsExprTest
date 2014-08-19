{-# LANGUAGE DeriveDataTypeable #-}

-- (c) 2012 Martin Jonáš

module Types.TypeExpression 
    ( TypeExpression(..)
    , TypeContext(..)
    , Type(..)
    , TypeClass(..)
    , TypeVariable(..)
    , TypeConstructor(..)
    ) where

import Data.Data ( Data )
import Data.Typeable ( Typeable )

-- | Data type TypeExpression represents parsed type expression
data TypeExpression = TypeExpression TypeContext Type
    deriving ( Show, Typeable, Data )

-- | Data type TypeContext represents parsed type context
data TypeContext = TypeContext [(TypeClass, TypeVariable)]
    deriving ( Show, Typeable, Data )

-- | Data type TypeExpression represents parsed type witnout the type context
data Type 
	-- | Application of the type constructor or variable on another type
	= TypeApplication Type Type
	-- | Type constructor
	| TypeConstructor TypeConstructor
	-- | Function between given two types
	| FunctionType Type Type
	-- | Type variable
	| VariableType TypeVariable
	-- | Tuple with values of types in given list in this order.
	| TupleType [Type]
	-- | List with elements of given type
	| ListType Type
	deriving ( Show, Eq, Typeable, Data )
	
-- | Data type TypeContext represents parsed type class
type TypeClass = String

-- | Data type TypeContext represents parsed type variable
type TypeVariable = String

-- | Data type TypeContext represents parsed type constructor
type TypeConstructor = String
