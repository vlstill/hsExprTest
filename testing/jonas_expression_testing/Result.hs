module Result
    ( CResult ( .. )
    , TestingResult ( .. )
    , TypingResult ( .. )
    ) where

import Types.TypeExpression ( TypeExpression )

data TestingResult
    = WontCompile String
    | NotTestable
    | TypesNotEqual TypingResult
    | DifferentValues
    | Success
    | Timeout
    deriving Show

data TypingResult
    = TypesEqual TypeExpression
    | CannotParse String
    | NotEqual String
    deriving Show

class CResult r where
    isSuccess :: r -> Bool

instance CResult TestingResult where
    isSuccess Success = True
    isSuccess _ = False

instance CResult TypingResult where
    isSuccess (TypesEqual _) = True
    isSuccess _              = False
