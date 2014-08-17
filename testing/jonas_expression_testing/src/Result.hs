{-# LANGUAGE DeriveDataTypeable #-}
module Result
    ( CResult ( .. )
    , TestingResult ( .. )
    , TypingResult ( .. )
    ) where

import Data.Monoid
import Types.TypeExpression ( TypeExpression )
import Data.Typeable ( Typeable )

data TestingResult
    = WontCompile String
    | NotTestable String
    | TypesNotEqual TypingResult
    | DifferentValues String
    | Success
    | Timeout
    | TestError String
    deriving ( Show, Typeable )

data TypingResult
    = TypesEqual TypeExpression
    | CannotParse String
    | NotEqual String
    deriving ( Show, Typeable )

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

