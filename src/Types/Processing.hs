{-# LANGUAGE TupleSections #-}

-- (c) 2012 Martin Jonáš
-- (c) 2014, 2015 Vladimír Štill

module Types.Processing ( normalize, normalizeAndSimplify ) where

import Types
import Data.List
import Control.Arrow

-- | -- | Function getTypeVariables returns type variables of given type
-- expressions in the order of the first occurance and numbers them with natural
-- numbers in this order.
normalizingSubstitution :: CType t => t -> Substitution
normalizingSubstitution = map (second (\x -> TypeVariable ("t" ++ show x))) . flip zip [1..] . nub . typeVars

-- | Function normalize normalizes given type expression.
-- | It substitutes all type variables with t1 ... tn in the order of the first
-- occurance and removes duplicate and nonused constexts.
normalize :: CType t => t -> t
normalize x = x // normalizingSubstitution x

-- | Normalize data type and try to simplify context (avoid unused type variables and duplicited in context)
normalizeAndSimplify :: TypeExpression -> TypeExpression
normalizeAndSimplify = simplify . normalize
  where
    simplify (TypeExpression (TypeContext con0) ty) = TypeExpression (TypeContext con) ty
      where
        con = filter (\x -> not $ null (concatMap typeVars (snd x) `intersect` vars)) (nub con0)
        vars = typeVars ty
