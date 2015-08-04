module Main ( main ) where

import Data.Bool
import Control.Applicative
import System.Exit

import Types.Parser
import Types.Formating

main = bool exitFailure exitSuccess . and =<< mapM runTest
       [ "a", "b", "blabla", "blaBla", "Int", "Double", "BlaBla"
       , "Maybe Integer", "IO a", "IO Int", "Either Int Bool"
       , "(Int, Bool)", "[String]", "[a]", "()", "([a], (b, Int))"
       , "a -> b", "b -> b", "a -> b -> c", "a -> Int", "r -> Int -> r"
       , "[a -> b]", "(a -> b) -> [a] -> [b]", "a -> b -> (a, b)", "(a, b) -> a"
       , "(a -> b) -> c", "(a -> b -> c) -> (d -> e) -> f"
       , "t a", "m b -> t a", "(a -> a) -> m a -> m a", "Maybe a -> Maybe b", "Maybe Int -> Maybe Integer"
       , "a (b c) d", "a b (c d)", "a (b c d)", "a (b (c d))"
       , "[t a]", "(t a, m a) -> a", "t a -> [a]"
       , "Ord a => a -> b", "(Ord a, Ord b) => (a, b) -> (a, b) -> Bool"
       , "Monad m => m a", "Monad m => m a -> (a -> m b) -> m b"
       , "(Functor f, Monad f, Default a) => f a"
--       , "Class a b => a b" -- multiparam classes not yet supported
--       , "(,) a", "(,,,) a b" -- we can't parse this yet, and it is obscure
       ]

runTest :: String -> IO Bool
runTest x = flip (either parseError) (parseType x) $ \ty -> do
                if formatType ty == x
                    then return True
                    else do
                        putStrLn $ "FAILED: " ++ formatType ty ++ " /= " ++ x
                        return False
  where
    parseError err = putStrLn ("FAILED: cannot parse '" ++ show x ++ "': " ++ show err) >> return False
