{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances
           , TemplateHaskell #-}

{- | Conversions for tests, this adds possiblity to covert lists of 'Fun'
     wrapped functions to lists of functions. Note that you should never need to
     use the 'Convertible' typeclass in you code and you cannot write new
     instances for it (to prevent unexpected behaviour in tests).
-}

-- (c) 2018 Vladimír Štill

module Test.QuickCheck.Convertible ( Convertible, convert ) where

import Test.QuickCheck.Function ( Fun (..) )
import Data.Coerce ( Coercible, coerce )
import Test.QuickCheck.GenConvertible ( convertibleN )

class Convertible a b where
    convert' :: a -> b

-- | base case, can be overridden by Fun or list case. This allows as to covert
-- between any coercible values
instance Coercible a b => Convertible a b where
    convert' = coerce

-- | Ideally this overload would be used only with the Fun overload, but since
-- the Coercible one is less specific it is impossible to give this one lower
-- priority, so we must give it higher to avoid abiguity.
instance {-# OVERLAPPING #-} Convertible a b => Convertible [a] [b] where
    convert' = map convert'

-- | Instances for QuickCheck generated functions, these have higher priority
-- then the Coercible ones.
instance {-# OVERLAPS #-} (Convertible a a', Convertible b b') => Convertible (Fun a' b) (a -> b') where
    convert' (Fun _ f) x = convert' (f (convert' x))

-- | Instances with automatic uncurring for higher arity functions
$(mapM convertibleN [2..16])

{-# INLINE convert #-}
convert :: Convertible a b => a -> b
convert = convert'
