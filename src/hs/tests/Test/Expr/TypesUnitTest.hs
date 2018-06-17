{-# LANGUAGE TemplateHaskell, LambdaCase, RankNTypes, KindSignatures #-}

-- (c) 2018 Vladimír Štill

module Test.Expr.TypesUnitTest where

import Test.Expr.Types
import Test.Expr.TypesTestUtils

test :: IO Bool
test = (fmap and . sequence)
    -- The testUnify function (splice) takes three parameters:  an expected
    -- result of type comparison and two types. The expected result is
    -- 'Either UniTypeId TypeOrder' and is a projection of the result of the
    -- 'unify' function to showing the first element of the tuples it returns.
    -- 'testUnify' creates an expression that runs comparsion of given two
    -- types, extracts the relevant results and matches them with the pattern.
    [ $(testUnify [p| Right TEqual |] [t| Int |] [t| Int |])
    , $(testUnify [p| Left _ |]       [t| Int |] [t| Double |])

    , $(testUnify [p| Right TLessGeneral |] [t| forall a  . a -> a |]
                                            [t| forall a b. a -> b |])

    , $(testUnify [p| Right TMoreGeneral |] [t| forall a b. a -> b |]
                                            [t| forall a  . a -> a |])

    -- substitute in one of the types for a concrete type
    , $(testUnify [p| Right TMoreGeneral |] [t| forall a b. a -> b   |]
                                            [t| forall a  . a -> Int |])

    , $(testUnify [p| Right TMoreGeneral |] [t| forall a b. a   -> b |]
                                            [t| forall b  . Int -> b |])

    , $(testUnify [p| Right TLessGeneral |] [t| forall b  . Int -> b |]
                                            [t| forall a b. a   -> b |])
    , $(testUnify [p| Right TLessGeneral |] [t| forall a.   a -> Int |]
                                            [t| forall a b. a -> b   |])

    -- here we have to substitue in both types
    , $(testUnify [p| Right TUnifiable |] [t| forall a. a -> a   |]
                                          [t| forall a. a -> Int |])

    , $(testUnify [p| Right TUnifiable |] [t| forall a. a -> a   |]
                                          [t| forall b. Int -> b |])

    , $(testUnify [p| Right TUnifiable |] [t| forall b. Int -> b |]
                                          [t| forall a. a   -> a |])

    , $(testUnify [p| Right TUnifiable |] [t| forall a. a -> Int |]
                                          [t| forall a. a -> a   |])

    -- (a -> a) vs (a -> b) -> (a -> b)
    , $(testUnify [p| Right TMoreGeneral |] [t| forall a  .  a       ->  a       |]
                                            [t| forall a b. (a -> b) -> (a -> b) |])

    , $(testUnify [p| Right TLessGeneral |] [t| forall a b. (a -> b) -> (a -> b) |]
                                            [t| forall a  .  a       -> a        |])

    -- occurs check
    , $(testUnify [p| Left BothTypes |] [t| forall a. a -> a |]
                                        [t| forall a. a -> a -> a |])

    , $(testUnify [p| Right TLessGeneral |] [t| Int |] [t| forall a. a |])
    , $(testUnify [p| Right TMoreGeneral |] [t| forall a. a |] [t| Int |])

    -- typeclasses
    , $(testUnify [p| Right TEqual |] [t| forall a. Integral a => a -> Int |]
                                      [t| forall b. Integral b => b -> Int |])

    , $(testUnify [p| Right TMoreGeneral |] [t| forall a.               a -> Int |]
                                            [t| forall b. Integral b => b -> Int |])

    , $(testUnify [p| Right TMoreGeneral |] [t| forall a.  Integral a            => a -> Int |]
                                            [t| forall b. (Integral b, Monoid b) => b -> Int |])

    , $(testUnify [p| Right TLessGeneral |] [t| forall a. Integral a => a -> Int |]
                                            [t| forall b.               b -> Int |])

    , $(testUnify [p| Right TLessGeneral |] [t| forall a. (Integral a, Monoid a) => a -> Int |]
                                            [t| forall b.  Integral b            => b -> Int |])

    , $(testUnify [p| Right TUnifiable |] [t| forall a. Integral a => a -> Int |]
                                          [t| forall b. Floating b => b -> Int |])

    -- type constructors
    , $(testUnify [p| Right TMoreGeneral |] [t| forall t a b. (a -> b) -> t a -> t b |]
                                            [t| forall   a b. (a -> b) -> [a] -> [b] |])

    , $(testUnify [p| Right TMoreGeneral |] [t| forall (t :: * -> *) a b. (a -> b) -> t a -> t b |]
                                            [t| forall               a b. (a -> b) -> [a] -> [b] |])

    {- this is a limitation: contexts cannot be simplified
    -- type constructors & typeclasses
    , $(testUnify [p| Right TMoreGeneral |] [t| forall t a b. Functor t => (a -> b) -> t a -> t b |]
                                            [t| forall   a b.              (a -> b) -> [a] -> [b] |])

    , $(testUnify [p| Right TMoreGeneral |] [t| forall (t :: * -> *) a b. Functor t => (a -> b) -> t a -> t b |]
                                            [t| forall               a b.              (a -> b) -> [a] -> [b] |])
    -}
    
    -- | check that RankNTypes fail
    , $(testUnify [p| Left LeftType |]  [t| (forall a. a -> a) -> Int |] [t| forall a b. a -> b |])
    , $(testUnify [p| Left RightType |] [t| forall a b. a -> b |] [t| (forall a. a -> a) -> Int |])
    ]
              
