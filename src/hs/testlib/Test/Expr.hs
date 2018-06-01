{-# LANGUAGE NamedFieldPuns, Unsafe, ExistentialQuantification, BangPatterns #-}

-- | Simple utility functions for testing.
--
-- (c) 2014-2018 Vladimír Štill

module Test.Expr ( (<==>), withTypeOf, testArgs, mainRunProperty ) where

import Test.QuickCheck ( Result (..), stdArgs, chatty, maxSuccess, replay, Property
                       , quickCheckWithResult, counterexample, Args )
import Test.QuickCheck.Random ( mkQCGen )

import Data.Typeable ( typeOf )
import Control.Exception ( SomeException ( SomeException ), Exception, catch )
import Control.DeepSeq ( rnf, ($!!), NFData )
import System.Exit ( exitSuccess, exitFailure )

import System.IO.Unsafe ( unsafePerformIO )

testArgs :: Args
testArgs = stdArgs { chatty = False
                   , maxSuccess = 1000
                   -- QC has no direct support for seeding, however,
                   -- replay also modifies size but it should only
                   -- (possibly) change size of the first testcase
                   , replay = Just (mkQCGen 0, 0)
                   }

mainRunProperty :: Args -> Property -> IO ()
mainRunProperty args prop = do
    r <- quickCheckWithResult args prop
    case r of
        Success {} -> exitSuccess
        GaveUp {} -> exitSuccess
        Failure { output } -> testFailure output
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
    handler (SomeException e) = return (Exc e)
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
