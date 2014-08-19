{-# LANGUAGE NamedFieldPuns
           , Unsafe
           , DeriveDataTypeable
           , StandaloneDeriving
           , ExistentialQuantification
           #-}
-- | Simple interface to testing
module Testing.Test (
    -- * Result re-exports
      TestingResult ( DifferentValues, Success, Timeout )
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
qcToResult (QCT.Failure { QCT.reason = r, QCT.output = o })
    = if "<<timeout>>" `isInfixOf` r then Timeout else DifferentValues o
qcToResult (QCT.NoExpectedFailure { QCT.output = o }) = DifferentValues o

firstFailed :: [ TestingResult ] -> TestingResult
firstFailed = mconcat

qcFirstFailed :: [ QCT.Result ] -> TestingResult
qcFirstFailed = firstFailed . map qcToResult

qcRunProperties :: [ AnyProperty ] -> IO TestingResult
qcRunProperties props = mapM applyQC props >>= return . qcFirstFailed
  where applyQC (AnyProperty p) = QCT.quickCheckWithResult (QCT.stdArgs { QCT.chatty = False }) p

(<==>) :: (Eq a, Show a) => a -> a -> QC.Property
infix 4 <==>
x <==> y = wrap x QC.=== wrap y
  where
    wrap x = unsafePerformIO $ (x `seq` return (OK x)) `catch` handler
    handler (SomeException e) = return $ Exc e

data Wrapper a
    = OK a
    | forall e. Exception e => Exc e

-- inline deriving does not work for existential types :-(
deriving instance Typeable Wrapper

instance Eq a => Eq (Wrapper a) where
    (OK x)  == (OK y)  = x == y
    (Exc x) == (Exc y) = typeOf x == typeOf y && show x == show y -- can't do really better here
    _       == _       = False

instance Show a => Show (Wrapper a) where
    show (OK a)  = show a
    show (Exc e) = "{ EXCEPTION THROWN (" ++ show (typeOf e) ++ "): " ++ show e ++ " }"
