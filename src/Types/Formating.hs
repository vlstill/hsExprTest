-- (c) 2014 Vladimír Štill

module Types.Formating ( FormatType ( formatType ), formatContext ) where

import Types.TypeExpression
import Data.List ( intercalate )

class FormatType t where
    formatType :: t -> String

instance FormatType Type where
    formatType (TypeApplication t1 t2) = _parens (formatType t1) ++ " " ++ _parens (formatType t2)
    formatType (TypeConstructor con)   = con
    formatType (FunctionType t1 t2) = (\x -> x ++ " -> " ++ formatType t2) (case t1 of
                                          FunctionType _ _ -> _parens (formatType t1)
                                          _                -> formatType t1)
    formatType (VariableType tvar) = tvar
    formatType (TupleType tts)     = _tuple (map formatType tts)
    formatType (ListType lt)       = '[' : formatType lt ++ "]"

instance FormatType TypeExpression where
    formatType (TypeExpression (TypeContext []) ty) = formatType ty
    formatType (TypeExpression con ty) = formatContext con ++ " => " ++ formatType ty
      where


formatContext :: TypeContext -> String
formatContext (TypeContext []) = "()"
formatContext (TypeContext [(c, v)]) = c ++ " " ++ v
formatContext (TypeContext cs) = _tuple (map (\(c, v) -> c ++ " " ++ v) cs)

_parens :: String -> String
_parens x = if ' ' `elem` x then '(' : x ++ ")" else x

_tuple :: [String] -> String
_tuple x = '(' : intercalate ", " x ++ ")"
