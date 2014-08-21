{-# LANGUAGE CPP #-}
-- (c) 2014 Vladimír Štill

module Compat.Read
     (
#if !MIN_VERSION_base(4,6,0)
     readEither, readMaybe,
#endif
     module Text.Read
     ) where

#if MIN_VERSION_base(4,6,0)
import Text.Read
#else
import qualified Text.Read as TR
import Data.Either
import Data.Maybe
import Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadPrec

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
-- A 'Left' value indicates a parse error.
--
-- Taken from base 4.7.0.1
readEither :: Read a => String -> Either String a
readEither s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
  where
    read' =
      do x <- TR.readPrec
         lift P.skipSpaces
         return x

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
--
-- Taken from base 4.7.0.1
readMaybe :: Read a => String -> Maybe a
readMaybe s = case readEither s of
                Left _  -> Nothing
                Right a -> Just a

#endif
