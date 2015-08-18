{-# LANGUAGE DataKinds
           , KindSignatures
           , PolyKinds
           , TypeOperators
           , GADTs
           , ExplicitForAll
           , ScopedTypeVariables
           #-}

-- (c) 2014 Vladimír Štill

module Modifiers
    ( Range, Ranges ( Range ), unRange
    , CRange ( toRanges )
    , inRanges
    ) where

import GHC.TypeLits
import Data.Proxy
import Control.Arrow
import System.Random

import Test.QuickCheck

-- | modifier for generating integral values within given inclusive range
type Range (i :: *) (from :: Nat) (to :: Nat) = Ranges i '[ '(from, to) ]

data Ranges :: * -> [ (Nat, Nat) ] -> * where
    Range :: (Integral i, Arbitrary i) => { unRange :: i } -> Ranges i ranges

instance Show i => Show (Ranges i ranges) where
    show = unRange >>> show

class CRange (a :: k) where
    toRanges :: Proxy a -> [(Integer, Integer)]

-- note that the tuple here in instance header is data constuctor lifted to type
instance forall from to. (KnownNat from, KnownNat to) => CRange '(from, to) where
    toRanges _ = [ (natVal (Proxy :: Proxy from), natVal (Proxy :: Proxy to)) ]

instance forall head tail. (CRange head, CRange tail) => CRange (head ': tail) where
    toRanges _ = toRanges (Proxy :: Proxy head) ++ toRanges (Proxy :: Proxy tail)

instance CRange '[] where
    toRanges _ = []

inRanges :: Integral i => i -> [(Integer, Integer)] -> Bool
inRanges val0 = any (\(x, y) -> val >= x && val <= y)
  where val = fromIntegral val0

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
