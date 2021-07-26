{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, UnicodeSyntax #-}
module Data.Data.Collect where

import Data.Data ( Data, cast, gmapQl )

{- | Collects all values of given type occuring as proper constituents of the
given value. The results are collected in a monoid (e.g. a list)

a simple example can be collection of values from nested lists:

>>> collectValues (\(x :: Int) -> [x]) [[1,2], [3], [4 :: Int]]
[1,2,3,4]

we cal also collect constituents of a recursive data structue itself:

>>> collectValues (\(x :: [Int]) -> [x]) [1, 2, 3, 4 :: Int]
[[2,3,4],[3,4],[4],[]]

For more complex exampe, let's have the following type:

@data BinTree a = N (BinTree a) a (BinTree a) | E deriving (Data, Typeable, Show)@

we can collect all values from the tree:

>>> collectValues (\(x :: Bool) -> [x]) $ N (N E True (N E False E)) True E
[True,False,True]

but we can also collect all subtrees:

>>> collectValues (\(x :: BinTree Bool) -> [x]) $ N (N E True (N E False E)) True E
[N E True (N E False E),E,N E False E,E,E,E]

-}
collectValues ∷ ∀ a b m. (Data a, Data b, Monoid m) ⇒ (b → m) → a → m
collectValues proj0 val0 = go val0
  where
    proj ∷ ∀ d. Data d ⇒ d → m
    proj val = case cast val of
                Nothing      → go val
                Just (v ∷ b) → proj0 v <> go val

    go val = gmapQl (<>) mempty proj val
