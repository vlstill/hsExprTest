{-# LANGUAGE LambdaCase #-}

-- | Parser for Haskell data types.
-- It should support most of Haslell 98 types, with exception
-- of freestanding list and typle constructors (such as in 'MaybeT []').
-- It also supports integral type literal exptesion of GHC (@-XTypeLits@).
-- It does not support multiparam type classec now.
--
-- * (c) 2012 Martin Jonáš
-- * (c) 2014,2015 Vladimír Štill

module Types.Parser ( parseType ) where

import Types
import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding (many, (<|>))
import Data.List

-- | Haskell identifier symbol
identSymbols :: Parser Char
identSymbols = alphaNum <|> oneOf "_'"

ident' :: Parser Char -> (Parser Char -> Parser String) -> Parser String
ident' front howmany = (:) <$> front <*> howmany identSymbols

ident :: Parser Char -> Parser String
ident front = ident' front many

spaces1 :: Parser ()
spaces1 = skipMany1 space <?> "white space"

-- | Type expression parser.
typeExpression :: Parser TypeExpression
typeExpression = TypeExpression <$> 
    (spaces *> (try ((parens typeContext <|> typeContext) <* spaced (string "=>")) <|> return (TypeContext [])) )
    <*> 
    (typeParser <* eof)

-- | Parser of one item of type context, for example - Num a
typeContextItem :: Parser (TypeClass, [Type])
typeContextItem = (,) <$> (typeClass <* spaces1) <*> (((:[]) . TypeVariable) <$> typeVariable)

-- | Parser of the entire type context, without surrounding parentheses.
typeContext :: Parser TypeContext
typeContext = TypeContext <$> (try (sepBy typeContextItem (try (spaced (char ',')))) <|> return [])

-- | Parser of type class. It may contain module part, for example Module.TypeClass.
typeClass :: Parser TypeClass
typeClass = intercalate "." <$> sepBy1 (ident upper) (char '.')

-- | Parser of type variable, basically parses string begining with lowercase
-- letter followed by arbitrary number of alphanumeric characters, underscored
-- and apostrophes, or string beginning with underscore with at leas one
-- aformentioned symbol following.
typeVariable :: Parser TypeVar
typeVariable = try (ident lower) <|> ident' (char '_') many1

-- | Parser of type constructor. Same as the type class parser, or numeric literal (TypeLits).
typeConstructor :: Parser TypeConstr
typeConstructor = (TyCon <$> typeClass) <|> (TyLit <$> many1 digit)

-- | Parser of function type.
typeParser :: Parser Type
typeParser = spaced $ chainr1 bTypeParser (string "->" *> return applyType)
  where
    applyType a b = (TypeConstructor FunTyCon `TypeApplication` a) `TypeApplication` b

-- | Parser of type application.
bTypeParser :: Parser Type
bTypeParser = spaced $ chainl1 aTypeParser (spaces *> return TypeApplication)

-- | Parser of other possibilities of the type syntax. Read type expression grammar for further details.
aTypeParser :: Parser Type
aTypeParser = spaced $ choice [ TypeConstructor <$> typeConstructor
        , try (TypeVariable <$> typeVariable)
        , try (string "()" *> return (TypeConstructor (TupleTyCon 0)))
        , try (parens typeParser)
        , try tupleParser
        , fmap (\ty -> TypeConstructor ListTyCon `TypeApplication` ty) (brackets typeParser) ]
  where
    tupleParser = do
        types <- parens (typeParser `sepBy` char ',')
        let len = length types
        return $ foldl TypeApplication (TypeConstructor (TupleTyCon len)) types

-- | Combinator "wrappning" given parser into parentheses with arbitrary number of spaces between parser and parentheses.
parens :: Parser a -> Parser a
parens = spaced . between (char '(' <* spaces) (spaces *> char ')')

-- | Combinator "wrappning" given parser into brackets with arbitrary number of spaces between parser and brackets.
brackets :: Parser a -> Parser a
brackets = spaced . between (char '[' <* spaces) (spaces *> char ']')

-- | Combinator padding given parser with spaces.
spaced :: Parser a -> Parser a
spaced = between spaces spaces

-- | Returns parse error or syntactic tree as a result.
parseType :: String -> Either ParseError TypeExpression
parseType = fmap normalize . parseType'

-- | Expand some known type synonyms
normalize :: TypeExpression -> TypeExpression
normalize (TypeExpression con typ) = TypeExpression con (nt typ)
  where
    nt :: Type -> Type
    nt ty = foldl' nt1 ty [ ( "String", lchar )
                          , ( "FilePath", lchar )
                          ]
    nt1 :: Type -> (String, Type) -> Type
    nt1 ty (match, replace) = foldType TypeApplication (\case
                                    TyCon x | x == match -> replace
                                    x                    -> TypeConstructor x
                                ) TypeVariable ty
    lchar :: Type
    lchar = TypeConstructor ListTyCon `TypeApplication` TypeConstructor (TyCon "Char")

parseType' :: String -> Either ParseError TypeExpression
parseType' = parse typeExpression "(unknown)"
