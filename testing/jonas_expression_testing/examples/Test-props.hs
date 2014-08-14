{-# LANGUAGE ScopedTypeVariables
           , Unsafe #-}
-- language extensions: you should specify Unsafe for the sake of clarity
-- but it should be implied by importing Teacher.Test (which is unsafe)
-- you must not specify Trustworthy or student's might be able to import
-- this module

-- This whole file will be loaded by expressionTesting program and interpreted
-- using GHC API much like if you load it into ghci (you must have expressionTesting
-- library installed in both cases)

-- usage:
-- $ ./expressionTesting --compare-expressions --testfile=examples/Test-props.hs --student="f x = x + 1"
--      ~~> TypesNotEqual ...
-- $ ./expressionTesting --compare-expressions --testfile=examples/Test-props.hs --student="f x = x"
--      ~~> Success

-- The module must be named Test
module Test where

-- you must import this and have testConfig
import Teacher.Test

-- inport student's module like this (safe keyword makes sure student's solution
-- is safe Haskell verified, you don't actually need to inport qualified
import safe qualified Student

-- this is configuration of test, see Teacher.Test for deails
-- other examples can be find in other files in examples/ folder
testConfig = defaultConfig
    -- specify that students function must have same type as an expression in this scope
    { expectedType = TypeOf "id"
    -- Example of test with properties, there can be any QuickCheck properties
    -- (typeclass Testable) hidden behind AnyProperty constructor (its a magick).
    --
    -- They will be tested in order and first failure will terminate test
    -- with error message.
    -- They must all compile though.
    , test = Properties
        [ AnyProperty (\(x :: Int) (y :: Int) -> x /= y || x == y)
        , AnyProperty (\(x :: Int) -> (fromIntegral x) + 1 == (fromIntegral (x + 1) :: Integer) )
        -- you can restrict polymorphic testacase like this (you will need ScopedTypeVariables
        -- language extension
        , AnyProperty (\(x :: Int) -> Student.f x == x )
        , AnyProperty (\(x :: Char) -> Student.f x == x )
        , AnyProperty (\(x :: [Integer]) -> Student.f x == x)
        ]
    , studentExpression = "f" -- the name of student's expression (unqualified)
    }
