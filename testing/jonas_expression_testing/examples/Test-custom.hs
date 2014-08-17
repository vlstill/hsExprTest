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
    -- you can disable extra type check here
    { expectedType = None
    -- use your function to run test, it can return either TestingResult
    -- or IO TestingResult (see module Testing.Test for documentation)
    -- we might get it to accept functions instead of string later
    , test = TestEntry "runtest"
    }

runtest :: IO TestingResult
runtest = do
    -- do some thesting using Student.f, which might be monadir
    -- you can use QuickCheck using quickCheckWithResult and Testing.Test.qc* functions
    return Success
    -- or
    return $ DifferentValues "message"
