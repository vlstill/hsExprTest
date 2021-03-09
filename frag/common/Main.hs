{-# LANGUAGE Unsafe, DeriveAnyClass, LambdaCase, DataKinds, GADTs,
             TemplateHaskell, TypeApplications, FlexibleContexts,
             ScopedTypeVariables, UnicodeSyntax, ConstraintKinds #-}
module Main ( main ) where

import Test ( tests )
import TestSpec ( TestSpec (..), RunCond (..), RunCondMode (..) )

import Test.Expr ( testArgs )
import Test.QuickCheck ( Result ( Success ), Property, maxSuccess,
                         quickCheckWithResult, output, within, labels, Args )
import Test.QuickCheck.Property ( callback, Callback ( PostTest ), CallbackKind ( NotCounterexample ), testCase, theException )

import Control.Monad.State
import Control.Lens hiding ( (<.>) )
import Control.Exception.Base ( Exception, SomeException (..), SomeAsyncException (..), fromException, throwIO, tryJust )
import Control.Monad.Catch ( MonadCatch, handle )
import Control.Concurrent ( myThreadId, throwTo )

import Data.List ( find, intercalate, sortOn )
import Data.Ord ( Down ( Down ) )
import Data.Default.Class
import Data.Maybe
import Data.Time.Clock.POSIX ( getPOSIXTime, POSIXTime )
import Data.Map ( Map, insert, insertWith, (!?) )

import System.Environment ( getArgs, lookupEnv )
import System.Exit ( exitSuccess, exitFailure )
import System.IO
import System.FilePath ( (<.>), (</>) )
import System.Posix.Signals ( installHandler, Handler( CatchOnce ), realTimeAlarm, scheduleAlarm )

import qualified Data.Map as Map


data GlobalTimeout = GlobalTimeout deriving (Show, Exception)

setupAlarm :: MonadIO m => Int -> m ()
setupAlarm secs = liftIO $ do
    mainTid <- myThreadId
    let alarmHandler = throwTo mainTid GlobalTimeout
    void $ installHandler realTimeAlarm (CatchOnce alarmHandler) Nothing
    void $ scheduleAlarm secs

data TestsState = TestsState { _failedTests :: Int
                             , _verbose :: Bool
                             , _totalTimeout :: Maybe POSIXTime
                             , _startTime :: POSIXTime
                             , _tested :: Map String Bool
                             , _runningInFrag :: Bool
                             , _testOutput :: Maybe Handle
                             , _currentTest :: Maybe String
                             , _currentFailed :: Bool
                             } deriving ( Show )
makeLenses ''TestsState

instance Default TestsState where
    def = TestsState { _failedTests = 0
                     , _verbose = False
                     , _totalTimeout = Nothing
                     , _startTime = 0
                     , _tested = mempty
                     , _runningInFrag = False
                     , _testOutput = Nothing
                     , _currentTest = Nothing
                     , _currentFailed = False
                     }

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runTests (map name tests)
        "--dump-tests":_ -> dumpTests
        xs -> runTests xs
  where
    runTests :: [String] -> IO ()
    runTests args = flip evalStateT def $ do
        let tot = length args
        forceVerbose <- isJust <$> liftIO (lookupEnv "VERBOSE")
        when (tot > 1 || forceVerbose) $ verbose .= True

        timeo <- fmap (read @Int) <$> liftIO (lookupEnv "TEST_TIMEOUT")
        -- we leave the last 200ms as a reserve to print messages before frag kills us
        totalTimeout .= fmap (subtract 0.2 . fromIntegral) timeo

        fragged <- isJust <$> liftIO (lookupEnv "FRAG_ASSIGNMENT")
        runningInFrag .= fragged

        start <- liftIO getPOSIXTime
        startTime .= start

        mapM_ runTestByName args

        failed <- use failedTests
        if failed == 0 then
            outLn ("Passed all " ++ show tot ++ " tests") >> liftIO exitSuccess
        else
            outLn ("Failed " ++ show failed ++ " of " ++ show tot ++ " tests") >> liftIO exitFailure

type MonadTest m = (MonadIO m, MonadCatch m, MonadState TestsState m)

remainingTime :: MonadTest m => m (Maybe Int)
remainingTime = use totalTimeout >>= \case
    Nothing -> pure $ Just maxBound
    Just timeo -> do
        start <- use startTime
        left <- (timeo -) . subtract start <$> liftIO getPOSIXTime
        if left >= 1 then
            pure . Just $ ceiling left
        else
            pure Nothing

withAlarm :: ∀m. MonadTest m => Int -> m Bool -> m Bool
withAlarm secs act = remainingTime >>= \case
    Nothing -> do
        outLn' "Skipped due to global timeout"
        pure False
    Just left -> handle lateAlarm $ do
        let alrm = min secs left
        setupAlarm alrm
        a <- act
        liftIO . void $ scheduleAlarm 0
        pure a
  where
    lateAlarm :: GlobalTimeout -> m Bool
    lateAlarm _ = do
        liftIO $ putStrLn "TIMEOUT hit just after the last test, test discarded"
        pure False


outWithIf :: MonadTest m => (String -> m ()) -> Getting Bool TestsState Bool -> String -> m ()
outWithIf doOut cond msg = use cond >>= \case
    True -> doOut msg
    _    -> pure ()

outLnIf, outIf :: MonadTest m => Getting Bool TestsState Bool -> String -> m ()
outLnIf = outWithIf outLn'
outIf = outWithIf out'

outLn, out :: MonadTest m => String -> m ()
outLn = outLnIf verbose
out = outIf verbose

outLn', out' :: MonadTest m => String -> m ()
outLn' = outWith hPutStrLn
out' = outWith hPutStr

outWith :: MonadTest m => (Handle -> String -> IO ()) -> String -> m ()
outWith hPut msg = do
    liftIO (hPut stdout msg >> hFlush stdout)
    use testOutput >>= \case
        Just h  -> liftIO (hPut h msg >> hFlush stdout)
        Nothing -> pure ()


reportTestFailure :: MonadState TestsState m => m ()
reportTestFailure = failedTests += 1 >> currentFailed .= True

runTestByName :: MonadTest m => String -> m ()
runTestByName testName = withTest testName $
                          case find (\t -> name t == testName) tests of
                            Nothing -> do
                                outLn' $ testName ++ "… not found"
                                reportTestFailure
                            Just t -> runTest t

withTest :: MonadTest m => String -> m () -> m ()
withTest testName act = do
    fragged <- use runningInFrag
    when fragged $ do
        oh <- liftIO $ openFile ("out" </> "test" <.> testName) WriteMode
        liftIO $ appendFile "out.list" ("test" <.> testName <> "\n")
        testOutput .= Just oh
    currentTest .= Just testName
    currentFailed .= False

    act

    currentTest .= Nothing
    when fragged $ do
        failed <- use currentFailed
        unless failed . liftIO $ writeFile ("passed" </> "test" <.> testName) ""
        use testOutput >>= liftIO . hClose . fromJust
        testOutput .= Nothing


testResult :: MonadTest m => String -> Bool -> m Bool
testResult testName res = tested %= insert testName res >> pure res

runTest :: ∀m. MonadTest m => TestSpec -> m ()
runTest test = do
    out $ name test ++ "… "
    maybe (out' "not implemented" >> reportTestFailure) evalTestCond $ property test
    outLn' . maybe "" (\x ->  " [" ++ x ++ "]\n") $ info test
  where
    args = case maxSucc test of
        Nothing -> testArgs
        (Just n) -> testArgs { maxSuccess = n }

    fmtCE = unlines . map ("    " ++) . filter (/= "(*)") . lines . output

    fmtLabel :: [String] -> Int -> String
    fmtLabel ls cnt = "[" ++ intercalate "+" ls ++ ": " ++ show cnt ++ "]"

    evalTestCond :: RunCond 'Exec (String, Property) String -> m ()
    evalTestCond prps = do
        res <- go prps
        unless res reportTestFailure
      where
        go :: RunCond 'Exec (String, Property) String -> m Bool
        go (And as)      = and <$> mapM go as
        go (Or  os)      = or  <$> mapM go os
        go (Fn p)        = uncurry eval p
        go (Depends t d) = checkDep d >>= \case
                            True  -> go t
                            False -> do
                                  skip t
                                  outLn' "Dependencies failed, this test is skipped"
                                  pure False

        checkDep :: RunCond 'Dep String String -> m Bool
        checkDep (Always x) = use (tested . to (!? x)) >>= \case
                    Nothing -> outLn' ("Dependency does not exits " <> x) >> pure False
                    Just r  -> unless r (outLn' $ "Failed dependency " <> x) >> pure r
        checkDep (And as)   = and <$> mapM checkDep as
        checkDep (Or os)    = or  <$> mapM checkDep os

        skip :: RunCond 'Exec (String, Property) String -> m ()
        skip (Fn (n, _))   = failIfUnset n
        skip (And as)      = mapM_ skip as
        skip (Or  os)      = mapM_ skip os
        skip (Depends t _) = skip t

        failIfUnset :: String -> m ()
        failIfUnset testName = tested %= insertWith (||) testName False

    eval :: String -> Property -> m Bool
    eval testName prp = withAlarm (testTimeoutSec test) $ do
        resOrTO <- liftIO $ quickCheckWithResultExc args (within (caseTimeoutMicroSec test) prp)
        case resOrTO of
            Right r@Success {} -> do
                let has_label = (> 1) . length $ labels r
                when has_label $ do
                    outIf (verbose . to not) $ name test ++ ": "
                    out' . unwords . map (uncurry fmtLabel) . sortOn (Down . snd) . Map.toList $ labels r
                    out' " "
                outLnIf (verbose . to (has_label ||)) "OK"
                testResult testName True
            Right r -> do
                outLn "FAILED:"
                outLn' $ fmtCE r
                testResult testName False
            Left timeout -> do
                outLn' "TIMEOUT on:"
                outLn' . unlines . map ("    " <>) $ input timeout
                testResult testName False

newtype Timeout = Timeout { input :: [String] } deriving (Show, Eq)
newtype TimeoutPropagator = TimeoutPropagator { getTimeout :: Timeout } deriving (Show, Exception)


quickCheckWithResultExc :: Args -> Property -> IO (Either Timeout Result)
quickCheckWithResultExc args prp = catchTimeout $ quickCheckWithResult args (callback cb prp)
  where
    cb = PostTest NotCounterexample handler
    handler _ result = when isTimeout $
            -- QuickCheck does not catch async exceptions so we use it here to get out
            throwIO . SomeAsyncException . TimeoutPropagator . Timeout $ testCase result
      where
        isTimeout = isJust $ theException result >>= fromException @GlobalTimeout

    catchTimeout = tryJust isAsyncTimeout
    isAsyncTimeout :: SomeAsyncException -> Maybe Timeout
    isAsyncTimeout (SomeAsyncException e) = getTimeout <$> fromException @TimeoutPropagator (SomeException e)


dumpTests :: IO ()
dumpTests = forM_ tests $ \test -> do
    putStrLn $ intercalate ", " [name test, show (pointWeight test)]
