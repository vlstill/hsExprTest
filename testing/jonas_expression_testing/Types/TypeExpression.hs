module Types.TypeExpression (TypeExpression(..), TypeContext(..), Type(..), TypeClass(..), TypeVariable(..), TypeConstructor(..)) where

-- | Data type TypeExpression represents parsed type expression
data TypeExpression = TypeExpression TypeContext Type deriving Show

-- | Data type TypeContext represents parsed type context
data TypeContext = TypeContext [(TypeClass, TypeVariable)] deriving Show

-- | Data type TypeExpression represents parsed type witnout the type context
data Type 
	-- | Application of the type on another type
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
	deriving (Show, Eq)
	
-- | Data type TypeContext represents parsed type class
type TypeClass = String

-- | Data type TypeContext represents parsed type variable
type TypeVariable = String

-- | Data type TypeContext represents parsed type constructor
type TypeConstructor = String