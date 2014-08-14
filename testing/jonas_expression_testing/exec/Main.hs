{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , DeriveDataTypeable
           , NamedFieldPuns
           #-}
module Main where

import System.Environment
import System.IO
import System.Exit
import Data.Maybe
import Data.List ( )
import Data.Data
import Data.Typeable
import Control.Monad

import System.Console.CmdLib hiding ( run )

import Result
import qualified Testing as Testing

data Main = Main { compareTypes       :: Bool
                 , compareExpressions :: Bool
                 , student            :: String
                 , solution           :: String
                 , limits             :: String
                 , expressionName     :: String
                 , testfile           :: FilePath
                 } deriving ( Typeable, Data, Eq, Show )

instance Attributes Main where
    attributes _ = group "Options" [
        compareTypes       %> Help "test type equality",
        compareExpressions %> Help "test expression equality",
        expressionName     %> [ Help "name of expression", ArgHelp "NAME" ],
        student            %> [ Help "student's answer", ArgHelp "CODE" ],
        solution           %> [ Help "teacher's solution", ArgHelp "CODE" ],
        limits             %> [ Help "time limits", ArgHelp "function1;function2;..." ],
        testfile           %> [ Help "theacher's file with predefined test", ArgHelp "PATH" ]
      ]

instance RecordCommand Main where
    mode_summary _ = "Expression testing for Haskell"

-- | Main function, behaves differently according to the command line arguments
main :: IO ()
main = getArgs >>= executeR (Main {}) >>= run

run :: Main -> IO ()
run m@(Main { compareTypes, compareExpressions }) = case (compareTypes, compareExpressions) of
    (True, True) -> hPutStrLn stderr "Could not do --compare-expressions and --compare-types at once" >>
                    exitWith (ExitFailure 1)
    (True, _   ) -> cTypes m
    (_   , True) -> cExpr m
    _            -> hPutStrLn stderr "You must specify either --compare-expressions or --compare-types"

-- | Function compareTypes gets student type and solution type from command line arguments and returns boolean value indicating equality of these two types
cTypes :: Main -> IO ()
cTypes (Main { student, solution }) = do
    let result = Testing.testTypeEquality solution student
    putStrLn $ if isSuccess result then "OK" else "FAILED: " ++ show result
    if isSuccess result then exitSuccess else exitWith (ExitFailure 32)

-- | Function compareTypes gets student expression and solution expression from command line arguments and returns boolean value indicating equality of these two expressions
cExpr :: Main -> IO ()
cExpr (Main { student, solution, testfile, limits, expressionName })
    = case (null testfile, null solution, null student, null expressionName) of
        (True, False, False, False) -> do
            result <- if null limits
                then Testing.compareExpressions expressionName solution student
                else Testing.compareLimitedExpressions (parseLimits limits) expressionName solution student
            finish result
        (False, True, False, True) -> Testing.runTestfile testfile student >>= finish
        _ -> do
            hPutStrLn stderr $ unlines [
              "You must specify `--student' and either of:",
              "    a) `--solution', `--expression-name' and optionally `--limits'",
              "    b) `--testfile'" ]
            exitWith (ExitFailure 1)
  where
    finish result = do
        case result of
            WontCompile m     -> putStrLn m
            DifferentValues v -> putStrLn $ "DifferentValues: " ++ v
            _                 -> putStrLn $ show result
        if isSuccess result then exitSuccess else exitWith (ExitFailure 32)

-- | Function parseLimits parses limiting expression in form "function;;;function;function;"
parseLimits :: String -> [Maybe String]
parseLimits limits = map (\xs -> if length xs == 0 then Nothing else Just xs) (split ';' limits)  

-- | Function split splits string in the given separator.
split :: Char -> String -> [String]
split = split' []

-- | Convience function
split' :: [String] -> Char -> String -> [String]
split' parts delimiter input = case (break (== delimiter) input) of
    (xs, []) -> parts ++ [xs]
    (xs, y:ys) -> split' (parts ++ [xs]) delimiter ys
