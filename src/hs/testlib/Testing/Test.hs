{-# LANGUAGE NamedFieldPuns, Unsafe, DeriveDataTypeable
           , ExistentialQuantification, Rank2Types, BangPatterns, CPP #-}

-- | Simple interface to testing.
--
-- (c) 2014,2015 Vladimír Štill

module Testing.Test (
    -- * Result re-exports
      TestResult(..)
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
    ) where

import Result

import qualified Test.QuickCheck as QC
import Test.QuickCheck.Random ( mkQCGen )
import qualified Test.QuickCheck.Test as QCT
import Test.QuickCheck ( Testable )

import Data.List
import Data.Typeable
import Control.Concurrent
import Control.Exception
import Control.DeepSeq

import System.IO.Unsafe ( unsafePerformIO )

-- | Wrapper for any value of 'Testable' typeclass
data AnyProperty = forall a. Testable a => AnyProperty a
    deriving ( Typeable )

-- | Format QuickCheck's 'QCT.Result' into 'TestResult'
qcToResult :: QCT.Result -> TestResult
qcToResult (QCT.Success {}) = Success
qcToResult (QCT.GaveUp {})  = Success
qcToResult (QCT.Failure { QCT.output = o, QCT.theException = exc }) = case exc of
    Just se -> case fromException se of
                 Just ex -> (ex :: AsyncException) `seq` Timeout (show ex)
                 _       -> TestFailure o
    Nothing -> TestFailure o
qcToResult (QCT.NoExpectedFailure { QCT.output = o }) = TestFailure o

-- | Get first failing result.
firstFailed :: [ TestResult ] -> TestResult
firstFailed = mconcat

-- | Get first failing result after conversion using 'qcToResult'
qcFirstFailed :: [ QCT.Result ] -> TestResult
qcFirstFailed = firstFailed . map qcToResult

-- | Run list of properties, possibly with limit.
qcRunProperties :: Maybe Int -> [ AnyProperty ] -> IO TestResult
qcRunProperties lim props = firstFailed <$> mapM (qcRunProperty lim) props

-- | Run property, possibly with limit.
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
                       -- QC has no direct support for seeding, however,
                       -- replay also modifies size but it should only
                       -- (possibly) change size of the first testcase
                       , QCT.replay = Just (mkQCGen 0, 0)
                       }

-- | Exception aware comparison, if no exception is thrown when evaluating
-- either of the values, compares them using '(==)', if exception is thrown
-- in both, exception type and message is compared, otherwise if exception
-- is thrown only in one, property fails. Mismatching values are returned
-- using 'QC.counterexample' on failure.
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
    rnf (Exc !_) = ()

-- | specalized version of 'const', returns first value and type is unified
-- with type of second value (which can be 'unified' as it is not evaluated).
withTypeOf :: a -> a -> a
withTypeOf = const
