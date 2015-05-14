{-# LANGUAGE LambdaCase #-}

-- (c) 2014,2015 Vladimír Štill

module Types.Formating ( FormatType ( formatType ), formatContext ) where

import Control.Applicative
import Control.Monad
import Types.TypeExpression
import Data.List ( intercalate )

class FormatType t where
    formatType :: t -> String

data Arg = Error
         | End
         | Val String
         | Fun (Maybe String -> Arg)


apply :: Arg -> Arg -> Arg
apply (Fun f)   (Val x) = f (Just x)
apply (Fun f)   End     = f Nothing
apply f@(Fun _) (Fun g) = f `apply` (g Nothing) 
apply _       _ = Error

sfun :: (String -> Arg) -> Arg
sfun f = Fun (\case Nothing -> Error; Just x -> f x)

unwrap :: Arg -> String
unwrap (Val x) = x
unwrap (Fun f) = unwrap (f Nothing)
unwrap _       = error "formatType: Invalid type"

instance FormatType Type where
    formatType = unwrap . foldType (\t1 t2 -> t1 `apply` t2) formatCon Val
      where
        formatCon :: TypeConstr -> Arg
        formatCon FunTyCon = sfun (\x -> sfun $ (\y -> Val $ _parens x ++ " -> " ++ _parens y))
        formatCon ListTyCon = sfun (\x -> Val $ "[" ++ x ++ "]")
        formatCon (TupleTyCon n) = aptuple [] n
        formatCon (TyCon con) = apcon con

        aptuple :: [String] -> Int -> Arg
        aptuple args 0 = Val $ "(" ++ intercalate ", " args ++ ")"
        aptuple args n = Fun $ \case
                            Nothing -> Val $ "(" ++ replicate (n + length args) ',' ++ ") " ++ intercalate " " (map _parens args)
                            Just v  -> aptuple (args ++ [v]) (n - 1)
        apcon :: String -> Arg
        apcon con = Fun $ \case
                      Nothing -> Val con
                      Just x  -> apcon $ con ++ " " ++ _parens x

instance FormatType TypeExpression where
    formatType (TypeExpression (TypeContext []) ty) = formatType ty
    formatType (TypeExpression con ty) = formatContext con ++ " => "++ formatType ty

formatContext :: TypeContext -> String
formatContext (TypeContext []) = "()"
formatContext (TypeContext [(c, v)]) = c ++ " " ++ _formatTList v
formatContext (TypeContext cs) = _tuple $ map (\(c, v) -> c ++ " " ++ _formatTList v) cs

_formatTList :: [Type] -> String
_formatTList = intercalate " " . map (_parens . formatType)

_parens :: String -> String
_parens x 
    | head x == '(' && last x == ')' = x
    | ' ' `elem` x                   = '(' : x ++ ")"
    | otherwise                      = x

_tuple :: [String] -> String
_tuple x = '(' : intercalate ", " x ++ ")"
