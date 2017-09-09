{-# LANGUAGE NamedFieldPuns, Unsafe, ExistentialQuantification, BangPatterns #-}

-- | Simple interface to testing.
--
-- (c) 2014-2017 Vladimír Štill

module Testing.Test (
    -- * Utility functions
       runProperty
    , mainRunProperty
    , (<==>)
    -- * Utility
    , AnyProperty ( AnyProperty )
    , withTypeOf
    ) where

import Test.QuickCheck ( Result (..), stdArgs, chatty, maxSuccess, replay, Property
                       , quickCheckWithResult, counterexample, Testable )
import Test.QuickCheck.Random ( mkQCGen )

import Data.List ( isInfixOf )
import Data.Typeable ( typeOf )
import Control.Concurrent ( myThreadId, threadDelay, killThread, forkIO )
import Control.Exception ( AsyncException ( UserInterrupt )
                         , SomeException ( SomeException )
                         , fromException, Exception, bracket, throwTo, catch
                         , throw )
import Control.DeepSeq ( rnf, ($!!), NFData )
import System.IO ( hPutStrLn, stderr )
import System.Exit ( exitSuccess, exitFailure )

import System.IO.Unsafe ( unsafePerformIO )

-- | Wrapper for any value of 'Testable' typeclass
data AnyProperty = forall a. Testable a => AnyProperty a

-- | Run property, possibly with limit.
runProperty :: Maybe Int -> AnyProperty -> IO Result
runProperty lim (AnyProperty p) = case lim of
    Nothing -> quickCheckWithResult args p
    Just limit -> do
        -- we are throwing UserInterrupt because it is the only exception
        -- which does not allow shrinking in QuickCheck (which is very
        -- important, as shirinking for example f = f will never end)
        -- also QuickCheck's within does not work either
        pid <- myThreadId
        bracket (forkIO (threadDelay limit >> throwTo pid UserInterrupt))
                killThread
                (const (quickCheckWithResult args p))
  where
    args = stdArgs { chatty = False
                   , maxSuccess = 1000
                   -- QC has no direct support for seeding, however,
                   -- replay also modifies size but it should only
                   -- (possibly) change size of the first testcase
                   , replay = Just (mkQCGen 0, 0)
                   }

mainRunProperty :: Int -> AnyProperty -> IO ()
mainRunProperty lim prop = do
    r <- runProperty (pure lim) prop
    case r of
        Success {} -> exitSuccess
        GaveUp {} -> exitSuccess
        Failure { output, theException } -> case theException >>= fromException of
                 Just ex -> (ex :: AsyncException) `seq` do
                                putStrLn $ "Timeout: " ++ show ex
                                hPutStrLn stderr "timeout"
                                exitFailure
                 _       -> testFailure output
        NoExpectedFailure { output } -> testFailure output
        _ -> do print r
                exitFailure
  where
    testFailure output = do putStrLn output
                            exitFailure

-- | Exception aware comparison, if no exception is thrown when evaluating
-- either of the values, compares them using '(==)', if exception is thrown
-- in both, exception type and message is compared, otherwise if exception
-- is thrown only in one, property fails. Mismatching values are returned
-- using 'QC.counterexample' on failure.
(<==>) :: (Eq a, Show a, NFData a) => a -> a -> Property
infix 4 <==>
x <==> y = x `comp` y
  where
    wrap v = unsafePerformIO $ (return . OK $!! v) `catch` handler
    handler se@(SomeException e) = case fromException se of
        Just ae -> throw (ae :: AsyncException)
        Nothing -> if "<<timeout>>" `isInfixOf` show e
                        then throw e
                        else return (Exc e)
    comp x0 y0 = counterexample (sx ++ " /= " ++ sy) (wx == wy)
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
