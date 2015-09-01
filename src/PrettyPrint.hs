-- | Typeclass for human readable formating of data types, together
-- with instances for Parsec's 'Message' and 'ParseError'.
--
-- * (c) 2015 Vladimír Štill

module PrettyPrint ( PrettyPrint(..) ) where


import Text.Parsec.Error
import Data.List
import Control.Arrow

-- | Human readable formating of data type
class PrettyPrint a where
    pp :: a -> String

instance PrettyPrint Message where
    pp = messageString

instance PrettyPrint ParseError where
    pp = errorMessages >>> map pp >>> intercalate "\n\n"

