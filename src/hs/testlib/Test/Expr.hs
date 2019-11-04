{-# LANGUAGE TemplateHaskell, ExistentialQuantification, Unsafe, BangPatterns
           , LambdaCase, NamedFieldPuns, RecordWildCards
           #-}

-- | Simple utility functions for testing.
--
-- (c) 2014-2018 Vladimír Štill

module Test.Expr (
                 -- * Test Entry
                 testMain, testMainEx,
                 testType, extractOptionDef, extractOptionMaybe, extractOption,
                 -- * Test Expression Building Blocks
                 (<==>), testArgs, runProperty, Args (..), scheduleAlarm
                 ) where

import Test.QuickCheck ( Result (..), stdArgs, chatty, maxSuccess, replay, Property
                       , quickCheckWithResult, counterexample, Args (..), Testable )
import Test.QuickCheck.Random ( mkQCGen )

import Data.Typeable ( typeOf )
import Data.Function ( (&) )
import Data.Maybe ( isNothing, isJust, fromMaybe, fromJust, catMaybes )

import Control.Exception ( SomeException ( SomeException ), Exception, catch, evaluate )
import Control.DeepSeq ( force, NFData )
import Control.Monad ( when )
import Control.Applicative ( (<|>) )

import System.Exit ( exitSuccess, exitFailure )
import Language.Haskell.TH ( Q, Exp (..), Dec (..), Clause (..), Body (..),
                             Lit (..), Pat, lookupValueName, mkName, Info (..),
                             Type, reify, tupleDataName )

import System.IO.Unsafe ( unsafePerformIO )
import System.Posix.Signals ( scheduleAlarm )

import Text.Printf.Mauke.TH ( sprintf )

import Test.Expr.Utils
import Test.Expr.Types
import Test.Expr.Property
import Test.Expr.Config

testArgs :: Args
testArgs = stdArgs { chatty = False
                   , maxSuccess = 1000
                   -- QC has no direct support for seeding, however,
                   -- replay also modifies size but it should only
                   -- (possibly) change size of the first testcase
                   , replay = Just (mkQCGen 0, 0)
                   }

testMain :: ExprName -> TypeOrder -> Maybe (Q Pat) -> Maybe (Q Type) -> Q [Dec]
testMain name typeOrder pat degen = testMainEx $ TestConfig conf
  where
    conf = ConfigEntry Expression name : ConfigEntry TypeOrd typeOrder : conf'
    conf' = catMaybes [ ConfigEntry DegenType <$> degen
                      , ConfigEntry TestPattern <$> pat
                      ]

testMainEx :: TestConfig -> Q [Dec]
testMainEx config = do
    studentExp' <- maybe unitExp (fmap (fmap VarE) . lookupValueName) sn
    $(pfail "Could not find student expression %s") name & when (isNothing studentExp')
    tname   <- maybe (pure Nothing) lookupValueName tn
    eval    <- lookupValueName "Teacher.evaluator"
    evalEx  <- lookupValueName "Teacher.evaluatorEx"
    $(pfail "Either teacher expression or evaluator has to be given for %s") name
         & when (isNothing (tname <|> eval <|> evalEx))
    let Just studentExp = studentExp'

    testBefore <- lookupValueName "Teacher.testBefore"
    $(pfail "Error: testBefore ignored in evaluator mode")
        & when (isJust (eval <|> evalEx) && isJust testBefore)
    let testBeforeExpr = case testBefore of
                            Nothing -> [| pure True |]
                            Just t -> pure $ VarE t

    let timeout = maybe defimeout tmout <$> lookupValueName "Teacher.timeout"
    comparer <- maybe defcmp VarE <$> lookupValueName "Teacher.comparer"
    let args = maybe defargs VarE <$> lookupValueName "Teacher.args"

    let mainName = mkName "main"
    mainType <- [t| IO () |]
    pattern <- sequence pat
    degenType <- sequence degen
    body <- case (evalEx, eval, tname) of
              (Just evx, _, _) -> [| scheduleAlarm $(timeout) >>
                                 $(pure $ VarE evx `AppE` liftSafeTH config `AppE` studentExp) |]
              (_, Just ev, _) -> [| scheduleAlarm $(timeout) >>
                                 $(pure $ VarE ev `AppE` studentExp) |]
              (_, _, Just teacherName) -> lookupValueName (fromJust sn) >>=
                                          \(Just studentName) -> -- used by {..}
                                          [| scheduleAlarm $(timeout) >>
                                          $(testBeforeExpr) >>= \tbr -> unless tbr exitFailure >>
                                          runProperty $(args) $(prop Prop {..}) |]
              _ -> fail "impossible"
    pure [ SigD mainName mainType
         , FunD mainName [Clause [] (NormalB body) []] ]
  where
    defimeout = LitE $ IntegerL 10
    tmout x = VarE 'fromIntegral `AppE` VarE x
    defcmp = VarE '(<==>)
    defargs = VarE 'testArgs
    tn = ("Teacher." ++) <$> mayName
    sn = ("Student." ++) <$> mayName
    name = fromMaybe "()" mayName
    unitExp = pure . Just . ConE $ tupleDataName 0

    mayName = config `getConfig` Expression
    Just typeOrder = config `getConfig` TypeOrd
    pat = config `getConfig` TestPattern
    degen = config `getConfig` DegenType

testType :: ExprName -> Q Exp
testType name = do
    st <- getType "student" =<< sequence . fmap reify =<< lookupValueName sn
    tt <- getType "teacher" =<< sequence . fmap reify =<< lookupValueName tn
    compareTypes tt st
  where
    getType kind Nothing = $(pfail "could not find %s type %s") kind name
    getType _ (Just (VarI _ t _)) = pure t
    getType kind _ = $(pfail "internal error while getting %s type %s") kind name
    tn = "Teacher." ++ name
    sn = "Student." ++ name

extractOptionDef :: String -> Exp -> Q Exp
extractOptionDef name def = maybe def VarE <$> lookupValueName ("Teacher." ++ name)

extractOptionMaybe :: String -> Q Exp
extractOptionMaybe name = maybe (ConE 'Nothing) ((ConE 'Just `AppE`) . VarE)
                                           <$> lookupValueName ("Teacher." ++ name)

extractOption :: String -> Q Exp
extractOption name = lookupValueName ("Teacher." ++ name) >>= \case
                        Nothing -> $(pfail "missing a mandatory option %s") name
                        Just x  -> pure $ VarE x

runProperty :: Testable prp => Args -> prp -> IO ()
runProperty args prp = do
    r <- quickCheckWithResult args prp
    case r of
        Success {}                      -> putStrLn "test passed" >> exitSuccess
        _                               -> testFailure (output r)
  where
    testFailure output = do putStrLn output
                            exitFailure

-- | Exception aware comparison. If no exception is thrown when evaluating
-- either of the values, compares them using '(==)', if exception is thrown
-- in both, exception type is compared, otherwise if exception is thrown only
-- in one, property fails. Mismatching values are returned using
-- 'QC.counterexample' on failure.
(<==>) :: (Eq a, Show a, NFData a) => a -> a -> Property
infix 4 <==>
x <==> y = x `comp` y
  where
    wrap v = unsafePerformIO $ (evaluate . OK $ force v) `catch` handler
    handler (SomeException e) = pure (Exc e)
    comp x0 y0 = counterexample (sx ++ " /= " ++ sy) (wx == wy)
      where
        wx = wrap x0
        wy = wrap y0
        sx = unwrap . wrap $ show wx
        sy = unwrap . wrap $ show wy
    unwrap  (OK str) = str
    unwrap ex@(Exc _) = show ex

data Wrapper a = OK !a
               | forall e. Exception e => Exc e

instance Eq a => Eq (Wrapper a) where
    (OK x)  == (OK y)  = x == y
    (Exc x) == (Exc y) = typeOf x == typeOf y
    _       == _       = False

instance Show a => Show (Wrapper a) where
    show (OK a)  = show a
    show (Exc e) = $(sprintf "{ EXCEPTION THROWN (%s): %s }") (show (typeOf e)) (show e)
