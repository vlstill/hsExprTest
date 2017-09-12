{-# LANGUAGE Unsafe #-}
module Test where

{-# LINE 1 "Test.hs" #-}
import qualified Student
import qualified Solution
import Prelude
import Data.Word
import Data.Int
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Function
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Range
import Testing.Test
import Types.Curry
import Control.DeepSeq

test :: AnyProperty
test = AnyProperty (\ x0 -> ((Student.foo) `withTypeOf` (undefined :: Integer -> Integer)) x0 <==> ((Solution.foo) `withTypeOf` (undefined :: Integer -> Integer)) x0 )

main :: IO ()
main = mainRunProperty test
