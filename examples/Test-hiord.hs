{-# LANGUAGE ScopedTypeVariables
           , Unsafe #-}
-- language extensions: you should specify Unsafe for the sake of clarity
-- but it should be implied by importing Testing.Test (which is unsafe)
-- you must not specify Trustworthy or student's might be able to import
-- this module

-- This whole file will be loaded by expressionTesting program and interpreted
-- using GHC API much like if you load it into ghci (you must have expressionTesting
-- library installed in both cases)

-- usage:
-- $ ./expressionTesting --compare-expressions --testfile=<path to this file> --student="<expression>"

-- The module must be named Test
module Test where

-- you must import this and have testConfig
import Testing.Test

-- you can of course import more QuickCheck
import Test.QuickCheck.Function

-- inport student's module like this (safe keyword makes sure student's solution
-- is safe Haskell verified, you don't actually need to inport qualified
import safe qualified Student

-- this is configuration of test, see Testing.Test for deails
-- other examples can be find in other files in examples/ folder
testConfig = defaultConfig
    -- specify that students function must have concrete type
    -- we can use this to verfy that type is as general as possible,
    -- properties are of course type checked
    { expectedType = Fixed "(a -> b) -> [a] -> [b]"
    -- Example of test with properties, there can be any QuickCheck properties
    -- (typeclass Testable) hidden behind AnyProperty constructor (its a magick).
    --
    -- They will be tested in order and first failure will terminate test
    -- with error message.
    -- They must all compile though.
    --
    -- This magical thing is how we can test higher order functions by having
    -- QuickCheck generate random functions for us!
    , test = Properties
        [ AnyProperty (\(Fun _ (f :: Int -> Int)) (x :: [Int]) -> Student.f f x == map f x )
        -- another way of specifying types
        , AnyProperty (\(Fun _ f) (x :: [Integer]) -> (Student.f f x :: [[Int]]) == map f x)
        -- sadly not everything is impemented in QuickCheck, so this won't compile
        -- , AnyProperty (\(Fun _ (f :: Double -> Int)) (x :: [Double]) -> Student.f f x == map f x )
        ]
    , studentExpression = "f" -- the name of student's expression (unqualified)
    }
