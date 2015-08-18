module Harness ( TestResult (..), ignored, runTests ) where

import Testing
import Types
import Result

import System.IO.Unsafe
import Data.IORef
import System.Exit
import Data.Data
import Control.Monad
import PrettyPrint

ignored :: a
ignored = undefined

runTests :: [ (String, String, String, TestResult ) ] -> IO ()
runTests tests = do
    r@(_, failed) <- foldM test (0, 0) tests
    putStrLn $ banner r
    if failed > 0 then exitFailure
                  else exitSuccess

banner (passed, 0) = "OK: Passed all " ++ show passed ++ " tests."
banner (passed, failed) = "FAIL: Passed " ++ show passed ++ " out of " ++ show (passed + failed) ++ " tests."

test :: (Integer, Integer) -> (String, String, String, TestResult) -> IO (Integer, Integer)
test (passed, failed) (student, solution, expr, expected) = do
    res <- runTest CompareExpressions { limit = Nothing
                                      , expressionName = expr
                                      , solution = solution
                                      , student = student
                                      , compareMode = FullComparison
                                      , typecheckMode = RequireTypeOrdering [ Equal ]
                                      }
    if toConstr res == toConstr expected
        then return (passed + 1, failed)
        else do
          putStrLn "---"
          putStrLn $ concat [ "Test ", student, " =?= ", solution, " ..." ]
          putStrLn "FAILED"
          putStrLn $ concat [ showConstr (toConstr res), " /= ", showConstr (toConstr expected) ]
          putStrLn $ if isSuccess res then "Unexpected success" else pp res
          return (passed, failed + 1)


