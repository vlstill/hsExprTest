module Types.Parser (parseType) where 

-- (c) 2012 Martin Jonáš
-- (c) 2014,2015 Vladimír Štill

import Types
import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding (many, (<|>))
import Data.List

-- | Type expression parser.
typeExpression :: Parser TypeExpression
typeExpression = TypeExpression <$> 
    (try ((parens typeContext <|> typeContext) <* spaced (string "=>")) <|> return (TypeContext [])) 
    <*> 
    (typeParser <* eof)

-- | Parser of one item of type context, for example - Num a
typeContextItem :: Parser (TypeClass, [Type])
typeContextItem = (,) <$> (typeClass <* spaces) <*> (((:[]) . TypeVariable) <$> typeVariable)

-- | Parser of the entire type context, without surrounding parentheses.
typeContext :: Parser TypeContext
typeContext = TypeContext <$> (try (sepBy typeContextItem (try (spaced (char ',')))) <|> return [])

-- | Parser of type class. It may contain module part, for example Module.TypeClass.
typeClass :: Parser TypeClass
typeClass = intercalate "." <$> sepBy1 ((:) <$> upper <*> many alphaNum) (char '.')

-- | Parser of type variable, basically parses string begining with lowercase letter followed by arbitrary number of alphanumeric characters.
typeVariable :: Parser TypeVar
typeVariable = (:) <$> lower <*> many alphaNum

-- | Parser of type constructor. Same as the type class parser, or numeric literal (TypeLits).
typeConstructor :: Parser TypeConstr
typeConstructor = TyCon <$> (typeClass <|> many1 digit)

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

-- | Function parseType runs the actual type parser and returns parse error or syntactic tree as a result.
parseType :: String -> Either ParseError TypeExpression
parseType = parse typeExpression "(unknown)"
