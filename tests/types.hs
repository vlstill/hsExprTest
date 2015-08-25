module Main ( main ) where

import Data.Bool
import Control.Applicative
import System.Exit

import Types.Parser
import Types

main = bool exitFailure exitSuccess . and =<< sequence (
    map runTest
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
       -- some parsing obscurities
       , "a", "a_a", "_a", "__", "_a_a", "aA", "a'", "a'a", "_'"
       , "A", "A_", "Maybe _a"
       , "AA_a a => a"
       -- type literals
       , "Range Int 0 1", "Range Int 0 1 -> Int -> Range Integer 0 1 -> Bool"
       ]
    ++
    map (uncurry runTest')
        [ ("a->b", "a -> b"), ("a->t a", "a -> t a"), ("a_->a'", "a_ -> a'"), ("a'->a'", "a' -> a'")
        , ("__->___", "__ -> ___"), ("A_b c=>c", "A_b c => c"), ("A __=>__->__", "A __ => __ -> __")
        , ("  a      ->  a    ", "a -> a"), ("   Ord       a   =>   a  ", "Ord a => a")
        ]
    )

runTest :: String -> IO Bool
runTest x = runTest' x x

runTest' :: String -> String -> IO Bool
runTest' x exp = flip (either parseError) (parseType x) $ \ty ->
                if formatType ty == exp
                    then return True
                    else do
                        putStrLn $ "FAILED: " ++ formatType ty ++ " /= " ++ exp
                        return False
  where
    parseError err = putStrLn ("FAILED: cannot parse '" ++ show x ++ "': " ++ show err) >> return False
