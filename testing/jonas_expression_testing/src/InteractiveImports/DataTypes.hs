{-# LANGUAGE Trustworthy #-}

module InteractiveImports.DataTypes (Nat(..), BinaryTree(..)) where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Control.Monad
import InteractiveImports.Limiting

-- | Data type Nat represents natural numbers (with zero)
data Nat = Zero | Succ Nat deriving (Eq, Ord, Show)

instance Arbitrary Nat where
    arbitrary = sized arbitrarySizedNat

instance Parametrizable Nat where
    parameter = natToInt

arbitrarySizedNat :: Int -> Gen Nat
arbitrarySizedNat n = fmap intToNat (choose (0, n))

-- | Function intToNat converts non-negative number into its Nat representation.
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n
    | n < 0 = error "Argument cannot be negative"
    | otherwise = Succ (intToNat (n - 1))

-- | Function natToInt converts Nat value into coresponding integer value.
natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ x) = natToInt x + 1

-- | Data type BinaryTree represents binary tree with labeled vertices.
data BinaryTree a = Empty | BinaryNode a (BinaryTree a) (BinaryTree a) deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (BinaryTree a) where
    arbitrary = sized arbitraryBinaryTree

instance Parametrizable (BinaryTree a) where
    parameter = binaryTreeNodes

arbitraryBinaryTree :: (Arbitrary a) => Int -> Gen (BinaryTree a)
arbitraryBinaryTree 0 = return Empty
arbitraryBinaryTree n
    | n > 0 = frequency [(1, return Empty),
                         (4, liftM3 BinaryNode arbitrary (arbitraryBinaryTree (n `div`2)) (arbitraryBinaryTree (n `div`2)))]

-- | Function binaryTreeNodes returns total number of nodes in given binary tree.
binaryTreeNodes :: BinaryTree a -> Int
binaryTreeNodes Empty = 0
binaryTreeNodes (BinaryNode _ left right) = 1 + binaryTreeNodes left + binaryTreeNodes right
