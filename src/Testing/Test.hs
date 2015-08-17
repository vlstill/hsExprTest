{-# LANGUAGE NamedFieldPuns, Unsafe, DeriveDataTypeable
           , ExistentialQuantification, Rank2Types, BangPatterns #-}

-- (c) 2014,2015 Vladimír Štill

-- | Simple interface to testing
module Testing.Test (
    -- * Result re-exports
      TestResult(..)
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
    , qcRunProperty
    , (<==>)
    -- * Utility
    , AnyProperty ( AnyProperty )
    , withTypeOf
    -- * QuickCheck instances and utilities
    , Arbitrary
    , CoArbitrary
    , Function
    , Fun ( Fun )
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
import Control.Concurrent
import Control.Exception
import Control.Applicative

import Prelude hiding ( catch )
import System.IO.Unsafe ( unsafePerformIO )
import Control.DeepSeq

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

qcToResult :: QCT.Result -> TestResult
qcToResult (QCT.Success {}) = Success
qcToResult (QCT.GaveUp {})  = Success
qcToResult (QCT.Failure { QCT.reason = r, QCT.output = o, QCT.theException = exc }) = case exc of
    Just se -> case fromException se of
                 Just ex -> (ex :: AsyncException) `seq` Timeout (show ex)
                 _       -> TestFailure o
    Nothing -> TestFailure o
qcToResult (QCT.NoExpectedFailure { QCT.output = o }) = TestFailure o

firstFailed :: [ TestResult ] -> TestResult
firstFailed = mconcat

qcFirstFailed :: [ QCT.Result ] -> TestResult
qcFirstFailed = firstFailed . map qcToResult

qcRunProperties :: Maybe Int -> [ AnyProperty ] -> IO TestResult
qcRunProperties lim props = firstFailed <$> mapM (qcRunProperty lim) props

qcRunProperty :: Maybe Int -> AnyProperty -> IO TestResult
qcRunProperty lim (AnyProperty p) = qcToResult <$> case lim of
    Nothing -> QCT.quickCheckWithResult args p
    Just limit -> do
        -- we are throwing UserInterrupt because it is the only exception
        -- which does not allow shrinking in QuickCheck (which is very
        -- important, as shirinking for example f = f will never end)
        -- also QuickCheck's within does not work either
        pid <- myThreadId
        bracket (forkIO (threadDelay limit >> throwTo pid UserInterrupt))
                killThread
                (const (QCT.quickCheckWithResult args p))
  where
    args = QCT.stdArgs { QCT.chatty = False
                       , QCT.maxSuccess = 1000
                       }

(<==>) :: (Eq a, Show a, NFData a) => a -> a -> QC.Property
infix 4 <==>
x <==> y = x `comp` y
  where
    wrap v = unsafePerformIO $ (return . OK $!! v) `catch` handler
    handler se@(SomeException e) = case fromException se of
        Just ae -> throw (ae :: AsyncException)
        Nothing -> if "<<timeout>>" `isInfixOf` show e
                        then throw e
                        else return (Exc e)
    comp x0 y0 = QC.counterexample (sx ++ " /= " ++ sy) (wx == wy)
      where
        wx = wrap x0
        wy = wrap y0
        sx = unwrap . wrap $ show wx
        sy = unwrap . wrap $ show wy
    unwrap  (OK str) = str
    unwrap ex@(Exc _) = show ex

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

instance NFData a => NFData (Wrapper a) where
    rnf (OK a)  = rnf a
    rnf (Exc !e) = ()

withTypeOf :: a -> a -> a
withTypeOf = const
