module Main ( main ) where

import Testing
import Result

import System.IO.Unsafe
import Data.IORef
import System.Exit
import Data.Data
import Control.Monad

ignored :: a
ignored = undefined

main :: IO ()
main = do
    r@(_, failed) <- foldM test (0, 0)
              [ ("f = 2^2^2", "f = (2^2)^2", "f", Success)
              -- , ("f x = x . ((.).)", "f = (.((.).))", "f", Success)
              , ("f m n = m `mod` n", "f = mod", "f", Success)
              , ("f _ 0 = 0; f m n = m `mod` n", "f = mod", "f", DifferentValues ignored )

              -- generate noncomutative functions
              , ( "f :: (a -> a -> a) -> a -> [a] -> a; f = foldl"
                , "f :: (a -> a -> a) -> a -> [a] -> a; f = foldr"
                , "f", DifferentValues ignored
                )
              -- indentation
              , ( "f :: a -> a -> a\nf x y = y"
                , "f :: a -> a -> a\nf = const"
                , "f", DifferentValues ignored
                )
              -- monomorphism restriction
              , ( "f x = x", "f = id", "f", Success )
              -- layout rule
              , ( "f = g\n  where g x = x", "f = id", "f", Success )
              -- higher order generated function, monomorphism restriction
              , ( unlines [ "f :: Integral b => ((Int -> b) -> b) -> Int"
                          , "f g = fromIntegral (g (\\x -> fromIntegral (x + 1)))" ]
                , unlines [ "f :: Integral i => ((Int -> i) -> i) -> Int"
                          , "f = fromIntegral . ($ (fromIntegral . (+ 1)))" ]
                , "f", Success
                )
              -- type mismatch
              , ( "f = const", "f :: a -> a -> a; f = undefined", "f", TypesNotEqual ignored )
              -- no IO supported
              , ( "f :: Int -> IO Int; f x = return x", "f :: Int -> IO Int; f = return"
                , "f", NotTestable ignored
                )
              -- wrapped function
              , ( "f :: Maybe (a -> a) -> a -> a; f Nothing x = x; f (Just g) x = g x"
                , "f :: Maybe (a -> a) -> a -> a; f Nothing = id; f (Just g) = g"
                , "f", Success
                )
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


