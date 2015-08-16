module PrettyPrint ( PrettyPrint(..) ) where

-- (c) 2015 Vladimír Štill

import Text.Parsec.Error
import Data.List
import Control.Arrow

class PrettyPrint a where
    pp :: a -> String

instance PrettyPrint Message where
    pp = messageString

instance PrettyPrint ParseError where
    pp = errorMessages >>> map pp >>> intercalate "\n\n"

