{-# LANGUAGE DataKinds
           , KindSignatures
           , GADTs
           , ExplicitForAll
           , ScopedTypeVariables
           #-}

module Modifiers
    ( Range ( Range ), unRange )
    where

import GHC.TypeLits
import Data.Proxy
import Control.Arrow
import System.Random

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

-- | modifier for generating integral values within given inclusive range
data Range :: * -> Nat -> Nat -> * where
    Range :: (Integral i, Arbitrary i) => { unRange  :: i } -> Range i from to

instance Show i => Show (Range i from to) where
    show = unRange >>> show

instance forall i from to.
         (Integral i, Arbitrary i, Random i, KnownNat from, KnownNat to)
         => Arbitrary (Range i (from :: Nat) (to :: Nat))
    where
        arbitrary = fmap Range $ choose (fromIntegral (natVal (Proxy :: Proxy from))
                                        , fromIntegral (natVal (Proxy :: Proxy to)))
        shrink = unRange >>>
                 shrinkIntegral >>>
                 filter (>= fromIntegral (natVal (Proxy :: Proxy from))) >>>
                 filter (<= fromIntegral (natVal (Proxy :: Proxy to))) >>>
                 map (Range :: i -> Range i from to)

