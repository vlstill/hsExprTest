{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , DeriveDataTypeable
           , NamedFieldPuns
           #-}
module UI ( runUI, runExpressionTester, Main ) where

import System.Environment
import System.IO
import System.Exit
import Data.Maybe
import Data.List ( )
import Data.Data
import Data.Typeable
import Control.Monad

import System.Console.CmdLib

import Result
import qualified Testing as Testing

data Main 
    = CompareTypes { student  :: String
                   , solution :: String
                   }
    | CompareExpressions { student        :: String
                         , solution       :: String
                         , expressionName :: String
                         , limits         :: String
                         }
    | Testfile { student  :: String
               , testfile :: FilePath
               }
    deriving ( Typeable, Data, Eq, Show, Read )



instance Attributes Main where

instance RecordCommand Main where
    mode_summary (CompareTypes { }) = "Compare types"
    mode_summary (CompareExpressions { }) = "Compare expressions"
    mode_summary (Testfile { }) = "Test student's solution using given testfile"

    mode_help _ = "mode_help"

    rec_options (CompareTypes { }) = group "Compare types" [
        student            %> [ Help "student's answer", ArgHelp "TYPE", Required True ],
        solution           %> [ Help "teacher's solution", ArgHelp "TYPE", Required True ]
      ]
    rec_options (CompareExpressions { }) = group "Compare expressions" [
        student            %> [ Help "student's answer", ArgHelp "CODE", Required True ],
        solution           %> [ Help "teacher's solution", ArgHelp "CODE", Required True ],
        expressionName     %> [ Help "name of expression", ArgHelp "NAME", Required True ],
        limits             %> [ Help "time limits", ArgHelp "function1;function2;...", Required False ]
      ]
    rec_options (Testfile { }) = group "Run testfile" [
        student            %> [ Help "student's answer", ArgHelp "CODE", Required True ],
        testfile           %> [ Help "theacher's file with predefined test", ArgHelp "PATH", Required True ]
      ]

    run' conf _ = runExpressionTester conf >>= \(success, msg) -> do
        putStrLn msg
        if success then exitSuccess else exitWith (ExitFailure 32)

runExpressionTester :: Main -> IO (Bool, String)
runExpressionTester (CompareTypes { student, solution }) = do
    let result = Testing.testTypeEquality solution student
    return (isSuccess result, if isSuccess result then "OK" else "FAILED: " ++ show result)

runExpressionTester (CompareExpressions { student, solution, expressionName, limits = [] }) =
    Testing.compareExpressions expressionName solution student >>= return . formatResult

runExpressionTester (CompareExpressions { student, solution, expressionName, limits }) =
    Testing.compareLimitedExpressions (parseLimits limits) expressionName solution student >>=
    return . formatResult

runExpressionTester (Testfile { student, testfile }) =
    Testing.runTestfile testfile student >>= return . formatResult

formatResult :: TestingResult -> (Bool, String)
formatResult (WontCompile m)     = (False, m)
formatResult (DifferentValues v) = (False, "DifferentValues: " ++ v)
formatResult result              = (isSuccess result, show result)

-- | Main function, behaves differently according to the command line arguments
runUI :: [ String ] -> IO ()
runUI = dispatch [] (recordCommands (undefined :: Main))
-- execute (recordCommands (error "a" :: Main))

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
