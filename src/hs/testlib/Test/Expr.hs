{-# LANGUAGE TemplateHaskell, ExistentialQuantification, NamedFieldPuns
           , Unsafe, BangPatterns
           #-}

-- | Simple utility functions for testing.
--
-- (c) 2014-2018 Vladimír Štill

module Test.Expr (
                 -- * Test Entry
                   testMain
                 -- * Test Expression Building Blocks
                 , (<==>), testArgs, runProperty, Args (..), scheduleAlarm
                 ) where

import Test.QuickCheck ( Result (..), stdArgs, chatty, maxSuccess, replay, Property
                       , quickCheckWithResult, counterexample, Args (..), Testable )
import Test.QuickCheck.Random ( mkQCGen )

import Data.Typeable ( typeOf )
import Control.Exception ( SomeException ( SomeException ), Exception, catch )
import Control.DeepSeq ( rnf, ($!!), NFData )
import System.Exit ( exitSuccess, exitFailure )
import Language.Haskell.TH ( Q, Exp (..), Lit (..), lookupValueName )

import System.IO.Unsafe ( unsafePerformIO )
import System.Posix.Signals ( scheduleAlarm )

import Test.Expr.Property

testArgs :: Args
testArgs = stdArgs { chatty = False
                   , maxSuccess = 1000
                   -- QC has no direct support for seeding, however,
                   -- replay also modifies size but it should only
                   -- (possibly) change size of the first testcase
                   , replay = Just (mkQCGen 0, 0)
                   }

type ExprName = String

testMain :: ExprName -> Q Exp
testMain name = do
    let timeout = maybe defimeout tmout <$> lookupValueName "Teacher.timeout"
    cmp <- maybe defcmp VarE <$> lookupValueName "Teacher.comparer"
    let args = maybe defargs VarE <$> lookupValueName "Teacher.args"
    [| scheduleAlarm $(timeout) >> runProperty $(args) $(sprop cmp tn sn) |]
  where
    defimeout = LitE $ IntegerL 10
    tmout x = VarE 'fromIntegral `AppE` VarE x
    defcmp = VarE '(<==>)
    defargs = VarE 'testArgs
    tn = "Teacher." ++ name
    sn = "Student." ++ name

runProperty :: Testable prp => Args -> prp -> IO ()
runProperty args prp = do
    r <- quickCheckWithResult args prp
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
