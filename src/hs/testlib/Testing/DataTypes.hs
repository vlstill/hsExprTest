{-# LANGUAGE Safe #-}

-- | Some usefull data types that can be used in tests.
--
-- (c) 2012 Martin Jonáš

module Testing.DataTypes (Nat(..), BinaryTree(..)) where

import Test.QuickCheck
import Control.Monad

-- | Data type Nat represents natural numbers (with zero)
data Nat = Zero | Succ Nat deriving (Eq, Ord, Show)

instance Arbitrary Nat where
    arbitrary = sized arbitrarySizedNat

arbitrarySizedNat :: Int -> Gen Nat
arbitrarySizedNat n = fmap intToNat (choose (0, n))

-- | Function intToNat converts non-negative number into its Nat representation.
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n
    | n < 0 = error "Argument cannot be negative"
    | otherwise = Succ (intToNat (n - 1))

-- | Data type BinaryTree represents binary tree with labeled vertices.
data BinaryTree a = Empty | BinaryNode a (BinaryTree a) (BinaryTree a) deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (BinaryTree a) where
    arbitrary = sized arbitraryBinaryTree

arbitraryBinaryTree :: (Arbitrary a) => Int -> Gen (BinaryTree a)
arbitraryBinaryTree 0 = return Empty
arbitraryBinaryTree n
    | n > 0 = frequency [(1, return Empty),
                         (4, liftM3 BinaryNode arbitrary (arbitraryBinaryTree (n `div`2)) (arbitraryBinaryTree (n `div`2)))]
    | otherwise = error "arbitraryBinaryTree: Negative value"

