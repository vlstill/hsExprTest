{-# LANGUAGE DeriveDataTypeable #-}

-- (c) 2012 Martin Jonáš
-- (c) 2014 Vladimír Štill

module Result
    ( CResult ( .. )
    , TestingResult ( .. )
    , TypingResult ( .. )
    ) where

import Data.Monoid
import Types.TypeExpression ( TypeExpression )
import Data.Typeable ( Typeable )
import Data.Data ( Data )

data TestingResult
    = WontCompile String
    | ExceptionWhileTesting String
    | NotTestable String
    | TypesNotEqual TypingResult
    | DifferentValues String
    | Success
    | TimeoutOrUserInterrupt
    | TestError String
    deriving ( Show, Typeable, Data )

data TypingResult
    = TypesEqual TypeExpression
    | CannotParse String
    | NotEqual String
    deriving ( Show, Typeable, Data )

class CResult r where
    isSuccess :: r -> Bool

instance CResult TestingResult where
    isSuccess Success = True
    isSuccess _ = False

instance CResult TypingResult where
    isSuccess (TypesEqual _) = True
    isSuccess _              = False

instance Monoid TestingResult where
    mempty = Success
    mappend Success y = y
    mappend x _       = x

