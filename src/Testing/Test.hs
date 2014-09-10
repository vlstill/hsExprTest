{-# LANGUAGE NamedFieldPuns
           , Unsafe
           , DeriveDataTypeable
           , ExistentialQuantification
           , Rank2Types
           #-}

-- (c) 2014 Vladimír Štill

-- | Simple interface to testing
module Testing.Test (
    -- * Result re-exports
      TestingResult ( DifferentValues, Success, TimeoutOrUserInterrupt )
    -- * Configuration
    , Expression
    , ExpectedType ( .. )
    , Test ( .. )
    , TestConfig ( .. )
    , defaultConfig
    -- * Utility functions
    , firstFailed
    , qcToResult
    , qcFirstFailed
    , qcRunProperties
    , (<==>)
    -- * Utility types
    , AnyProperty ( AnyProperty )
    -- * QuickCheck instances and utilities
    , Arbitrary
    , CoArbitrary
    , Function
    , Fun ( Fun )
    -- * InteractiveImports re-export
    , module Testing.DataTypes
    , module Testing.Limiting
    ) where

import Result
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Test as QCT
import Test.QuickCheck.Arbitrary ( Arbitrary, CoArbitrary )
import Test.QuickCheck.Function ( Function, Fun ( Fun ) )
import Control.Monad ( mapM )
import Test.QuickCheck ( Testable )
import Data.Monoid
import Data.List
import Data.Typeable
import Testing.DataTypes
import Testing.Limiting
import Control.Concurrent
import Control.Exception

import Prelude hiding ( catch )
import System.IO.Unsafe ( unsafePerformIO )
import Control.Exception

data AnyProperty = forall a. Testable a => AnyProperty a
    deriving ( Typeable )

type Expression = String

data ExpectedType
    = TypeOf { etTypeOf :: Expression }
    | Fixed { etFixed :: String }
    | None
    deriving ( Typeable, Show )

data Test
    = TestEntry { tTestEntry :: String }
    | Properties { tProperties :: [ AnyProperty ] }
    deriving ( Typeable )

data TestConfig = TestConfig
    { expectedType      :: ExpectedType
    , studentExpression :: String
    , test              :: Test
    } deriving ( Typeable )

defaultConfig :: TestConfig
defaultConfig = TestConfig { expectedType = None
                           , studentExpression = "f"
                           , test = Properties []
                           }

qcToResult :: QCT.Result -> TestingResult
qcToResult (QCT.Success {}) = Success
qcToResult (QCT.GaveUp {})  = Success
qcToResult (QCT.Failure { QCT.reason = r, QCT.output = o, QCT.theException = exc }) = case exc of
    Just se -> case fromException se of
                 Just ex -> (ex :: AsyncException) `seq` TimeoutOrUserInterrupt
                 _       -> DifferentValues o
    Nothing -> DifferentValues o
qcToResult (QCT.NoExpectedFailure { QCT.output = o }) = DifferentValues o

firstFailed :: [ TestingResult ] -> TestingResult
firstFailed = mconcat

qcFirstFailed :: [ QCT.Result ] -> TestingResult
qcFirstFailed = firstFailed . map qcToResult


qcRunProperties :: Maybe Int -> [ AnyProperty ] -> IO TestingResult
qcRunProperties lim props = mapM applyQC props >>= return . qcFirstFailed
  where
    applyQC (AnyProperty p) = case lim of
        Nothing -> QCT.quickCheckWithResult args p
        Just limit -> do
            -- we are throwing UserInterrupt because it is the only exception
            -- which does not allow shrinking in QuickCheck (which is very
            -- important, as shirinking for example f = f will never end)
            -- also QuickCheck's within does not work either
            pid <- myThreadId
            bracket (forkIO (threadDelay limit >> throwTo pid UserInterrupt))
                    (killThread)
                    (const (QCT.quickCheckWithResult args p))
    args = QCT.stdArgs { QCT.chatty = False
                       , QCT.maxSuccess = 1000
                       }

(<==>) :: (Eq a, Show a) => a -> a -> QC.Property
infix 4 <==>
x <==> y = wrap x QC.=== wrap y
  where
    wrap x = unsafePerformIO $ (x `seq` return (OK x)) `catch` handler
    handler se@(SomeException e) = case fromException se of
        Just ae -> throw (ae :: AsyncException)
        Nothing -> if "<<timeout>>" `isInfixOf` show e
                        then throw e
                        else return (Exc e)

data Wrapper a
    = OK a
    | forall e. Exception e => Exc e

instance Eq a => Eq (Wrapper a) where
    (OK x)  == (OK y)  = x == y
    (Exc x) == (Exc y) = typeOf x == typeOf y && show x == show y -- can't do really better here
    _       == _       = False

instance Show a => Show (Wrapper a) where
    show (OK a)  = show a
    show (Exc e) = "{ EXCEPTION THROWN (" ++ show (typeOf e) ++ "): " ++ show e ++ " }"
