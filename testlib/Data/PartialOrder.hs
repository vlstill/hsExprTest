{-# LANGUAGE DefaultSignatures, Safe #-}

-- (c) 2018-2021 Vladimír Štill

module Data.PartialOrder where

import Data.Maybe ( isJust )

-- | Partially ordered values. The default instance just lifts 'compare' to
-- 'pcompare', i.e. all values should be comparable.
class PartialOrder a where
    pcompare :: a -> a -> Maybe Ordering
    comparable :: a -> a -> Bool
    lte :: a -> a -> Bool
    gte :: a -> a -> Bool
    lt :: a -> a -> Bool
    gt :: a -> a -> Bool
    eq :: a -> a -> Bool

    {-# MINIMAL #-} -- minimal instance is the one derived by default

    comparable x y = isJust $ pcompare x y
    lte x y = case pcompare x y of
                Just LT -> True
                Just EQ -> True
                _       -> False
    gte x y = case pcompare x y of
                Just GT -> True
                Just EQ -> True
                _       -> False
    lt x y = case pcompare x y of
                Just LT -> True
                _       -> False
    gt x y = case pcompare x y of
                Just GT -> True
                _       -> False
    eq x y = case pcompare x y of
                Just EQ -> True
                _       -> False

    default pcompare :: Ord a => a -> a -> Maybe Ordering
    pcompare x y = Just $ compare x y


instance PartialOrder Bool where
instance PartialOrder Char where
instance PartialOrder Int where
instance PartialOrder Integer where
instance PartialOrder Word where
instance PartialOrder Float where
instance PartialOrder Double where
instance PartialOrder () where

instance (PartialOrder a, PartialOrder b) => PartialOrder (a, b) where
    pcompare (x, y) (a, b) = case pcompare x a of
                              Nothing -> Nothing
                              Just EQ -> pcompare y b
                              Just c  -> Just c

instance (PartialOrder a, PartialOrder b, PartialOrder c) => PartialOrder (a, b, c) where
    pcompare (x1, x2, x3) (y1, y2, y3) = pcompare (x1, (x2, x3)) (y1, (y2, y3))

instance (PartialOrder a, PartialOrder b, PartialOrder c, PartialOrder d) => PartialOrder (a, b, c, d) where
    pcompare (x1, x2, x3, x4) (y1, y2, y3, y4) = pcompare (x1, (x2, (x3, x4))) (y1, (y2, (y3, y4)))

instance PartialOrder a => PartialOrder [a] where
    pcompare [] []         = Just EQ
    pcompare (x:xs) (y:ys) = case pcompare x y of
                              Nothing -> Nothing
                              Just EQ -> pcompare xs ys
                              Just c  -> Just c
    pcompare [] (_:_)       = Just LT
    pcompare (_:_) []       = Just GT
