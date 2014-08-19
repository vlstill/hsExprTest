module Main ( main ) where

import Testing
import Result

import System.IO.Unsafe
import Data.IORef
import System.Exit
import Data.Data
import Control.Monad

main :: IO ()
main = do
    r@(_, failed) <- foldM test (0, 0)
              [ ("f = 2^2^2", "f = (2^2)^2", "f", Success)
              -- , ("f x = x . ((.).)", "f = (.((.).))", "f", Success)
              , ("f m n = m `mod` n", "f = mod", "f", Success)
              , ("f _ 0 = 0; f m n = m `mod` n", "f = mod", "f", DifferentValues "" )
              ]
    putStrLn $ banner r
    if failed > 0 then exitFailure
                 else exitSuccess

banner (passed, 0) = "OK: Passed all " ++ show passed ++ " tests."
banner (passed, failed) = "FAIL: Passed " ++ show passed ++ " out of " ++ show (passed + failed) ++ " tests."

test :: (Integer, Integer) -> (String, String, String, TestingResult) -> IO (Integer, Integer)
test (passed, failed) (student, solution, expr, expected) = do
    putStrLn $ concat [ "Testing ", student, " =?= ", solution, " ..." ]
    res <- compareExpressions expr solution student
    if toConstr res == toConstr expected
        then putStrLn "OK" >> return (passed + 1, failed)
        else do
          putStrLn "FAILED"
          putStrLn $ concat [ showConstr (toConstr res), " /= ", showConstr (toConstr expected) ]
          putStrLn $ show res
          return (passed, failed + 1)


