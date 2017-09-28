{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, TypeOperators, GADTs
           , ExplicitForAll, ScopedTypeVariables, FlexibleInstances
           , Safe #-}


-- | Entenstion of QuickCheck's modifiers with Integral ranges with type
-- defined bounds.
--
-- >>> foo :: Range Int 0 255 -> Bool
-- >>> foo (Range v) = /* ... */
--
-- (c) 2014 Vladimír Štill

module Test.QuickCheck.Range
    ( Range, Ranges ( Range, unRange )
    , CharRanges ( CharRange ), unCharRange, charRangeToRange, CharRange
    , CRange ( toRanges )
    , inRanges
    ) where

import GHC.TypeLits
import Data.Proxy
import Data.Char ( chr, ord )
import Control.Arrow
import System.Random

import Test.QuickCheck

-- | Modifier for generating integral values within given inclusive range.
--
-- @Range Int 0 42@ will have arbitrary values of type 'Int' in range from
-- 0 to 42 inclusive.
type Range (i :: *) (from :: Nat) (to :: Nat) = Ranges i '[ '(from, to) ]

-- | Modifier for choosing arbitrarily from multiple ranges (first range
-- is chosen uniformly, than value from this range is chosen uniformly).
--
-- For example, @Ranges Int [(0,0), (10, 19)]@ will with probability @1/2@
-- generate @0@, and with probability @1/20@ one of (10, 19) inclusive.
data Ranges :: * -> [ (Nat, Nat) ] -> * where
    Range :: { unRange :: i } -> Ranges i ranges

-- | Show instance is transparent.
instance Show i => Show (Ranges i ranges) where
    show = unRange >>> show

-- | Convert compile time type ranges to runtime values.
class CRange (a :: k) where
    toRanges :: Proxy a -> [(Integer, Integer)]

-- note that the tuple here in instance header is data constuctor lifted to type
instance forall from to. (KnownNat from, KnownNat to) => CRange '(from, to) where
    toRanges _ = [ (natVal (Proxy :: Proxy from), natVal (Proxy :: Proxy to)) ]

instance forall head tail. (CRange head, CRange tail) => CRange (head ': tail) where
    toRanges _ = toRanges (Proxy :: Proxy head) ++ toRanges (Proxy :: Proxy tail)

instance CRange '[] where
    toRanges _ = []

-- must not cause value to go out of range
unsafeRMap :: (a -> b) -> Ranges a ranges -> Ranges b ranges
unsafeRMap f (Range x) = Range (f x)

-- | is given value in range?
inRanges :: Integral i => i -> [(Integer, Integer)] -> Bool
inRanges val0 = any (\(x, y) -> val >= x && val <= y)
  where val = fromIntegral val0

-- | it would seem like the OVERLAPPABLE is redundant here, but GHC 8.2 has
-- problem with Ranges Char instances otherwise
instance forall i ranges.
         (Integral i, Arbitrary i, Random i, CRange ranges)
         => Arbitrary (Ranges i ranges)
    where
        arbitrary = map (fromIntegral *** fromIntegral >>> choose) >>>
                    oneof >>> fmap Range $ toRanges (Proxy :: Proxy ranges)

        shrink = unRange >>>
                 shrinkIntegral >>>
                 filter (flip inRanges (toRanges (Proxy :: Proxy ranges))) >>>
                 map Range

newtype CharRanges ranges = CharRange { charRangeToRange :: Ranges Char ranges }
type CharRange (from :: Nat) (to :: Nat) = CharRanges '[ '(from, to) ]

unCharRange :: CharRanges ranges -> Char
unCharRange (CharRange r) = unRange r

instance Show (CharRanges ranges) where
    show = unCharRange >>> show

instance forall ranges. CRange ranges => Arbitrary (CharRanges ranges) where
    arbitrary = fmap (CharRange . unsafeRMap chr) (arbitrary :: Gen (Ranges Int ranges))
    shrink = charRangeToRange >>> unsafeRMap ord >>> shrink >>> map (CharRange . unsafeRMap chr)
