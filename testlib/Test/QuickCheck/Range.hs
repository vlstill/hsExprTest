{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, TypeOperators, GADTs
           , ExplicitForAll, ScopedTypeVariables, FlexibleInstances
           , LambdaCase, Safe, TypeApplications #-}


-- | Entenstion of QuickCheck's modifiers with Integral ranges with type
-- defined bounds.
--
-- >>> foo :: Range Int 0 255 -> Bool
-- >>> foo (Range v) = /* ... */
--
-- (c) 2014-2019 Vladimír Štill

module Test.QuickCheck.Range
    ( Range, Ranges ( Range, unRange )
    , CharRanges ( CharRange ), unCharRange, charRangeToRange, CharRange, AsciiPrintableRange
    , CRange ( toRanges )
    , inRanges
    , BoundedList ( BoundedList ), unBoundedList
    ) where

import GHC.TypeLits
import Data.Proxy
import Data.Char ( chr, ord )
import Data.Kind ( Type )
import Data.Function ( on )
import Control.Arrow
import Control.DeepSeq
import System.Random

import Test.QuickCheck

-- | Modifier for generating integral values within given inclusive range.
--
-- @Range Int 0 42@ will have arbitrary values of type 'Int' in range from
-- 0 to 42 inclusive.
type Range (i :: Type) (from :: Nat) (to :: Nat) = Ranges i '[ '(from, to) ]

-- | Modifier for choosing arbitrarily from multiple ranges (first range
-- is chosen uniformly, than value from this range is chosen uniformly).
--
-- For example, @Ranges Int [(0,0), (10, 19)]@ will with probability @1/2@
-- generate @0@, and with probability @1/20@ one of (10, 19) inclusive.
newtype Ranges :: Type -> [ (Nat, Nat) ] -> Type where
    Range :: { unRange :: i } -> Ranges i ranges

-- | Show instance is transparent.
instance Show i => Show (Ranges i ranges) where
    show = unRange >>> show

instance Eq i => Eq (Ranges i ranges) where
    (==) = (==) `on` unRange

instance Ord i => Ord (Ranges i ranges) where
    compare = compare `on` unRange

instance NFData i => NFData (Ranges i ranges) where
    rnf (Range v) = rnf v

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
        arbitrary = Range <$> sized (flip gen ranges)
          where
            gen :: Int -> [(i, i)] -> Gen i
            gen n
             | n <= 5    = let s = fromIntegral n
                               options a b = filter (\x -> a <= x && x <= b)
                                                    ([a .. (a + s)] ++ [(b - s) .. b])
                           in concatMap (uncurry options) >>> elements
             | otherwise = map choose >>> oneof

            ranges :: [(i, i)]
            ranges = map (fromIntegral *** fromIntegral) $ toRanges (Proxy :: Proxy ranges)

        shrink = unRange >>>
                 shrinkIntegral >>>
                 filter (`inRanges` toRanges (Proxy :: Proxy ranges)) >>>
                 map Range

instance CoArbitrary i => CoArbitrary (Ranges i r) where
    coarbitrary (Range v) = coarbitrary v

instance forall i r. Function i => Function (Ranges i r) where
    function = functionMap unRange (Range @i @r)

newtype CharRanges ranges = CharRange { charRangeToRange :: Ranges Char ranges }
type CharRange (from :: Nat) (to :: Nat) = CharRanges '[ '(from, to) ]

unCharRange :: CharRanges ranges -> Char
unCharRange (CharRange r) = unRange r

instance Show (CharRanges ranges) where
    show = unCharRange >>> show

instance Eq (CharRanges ranges) where
    (==) = (==) `on` (unRange . charRangeToRange)

instance Ord (CharRanges ranges) where
    compare = compare `on` (unRange . charRangeToRange)

instance NFData (CharRanges ranges) where
    rnf (CharRange v) = rnf v

instance forall ranges. CRange ranges => Arbitrary (CharRanges ranges) where
    arbitrary = fmap (CharRange . unsafeRMap chr) (arbitrary :: Gen (Ranges Int ranges))
    shrink = charRangeToRange >>> unsafeRMap ord >>> shrink >>> map (CharRange . unsafeRMap chr)

instance CoArbitrary (CharRanges r) where
    coarbitrary (CharRange v) = coarbitrary v

instance forall r. Function (CharRanges r) where
    function = functionMap charRangeToRange (CharRange @r)

newtype BoundedList (a :: Type) (from :: Nat) (to :: Nat) = BoundedList { unBoundedList :: [a] }
        deriving ( Eq, Ord )

instance Show a => Show (BoundedList a from to) where
    show = show . unBoundedList

intNatVal :: KnownNat x => Proxy x -> Int
intNatVal p = fromIntegral $ natVal p

instance forall a from to. (Arbitrary a, KnownNat from, KnownNat to) => Arbitrary (BoundedList a from to) where
    arbitrary = choose (intNatVal (Proxy :: Proxy from), intNatVal (Proxy :: Proxy to)) >>= \l ->
                BoundedList <$> vectorOf l arbitrary

    shrink (BoundedList xs) = map BoundedList . filter ((>= intNatVal (Proxy :: Proxy from)) . length) $ shrink xs

instance CoArbitrary a => CoArbitrary (BoundedList a from to) where
    coarbitrary (BoundedList bl) = coarbitrary bl

instance forall a from to. Function a => Function (BoundedList a from to) where
    function = functionMap unBoundedList (BoundedList @a @from @to)

type AsciiPrintableRange = CharRange 32 126
