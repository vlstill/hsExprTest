{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable 
           , NamedFieldPuns, DisambiguateRecordFields #-}

-- (c) 2012 Martin Jonáš
-- (c) 2014,2015 Vladimír Štill

module UI ( runUI, runExpressionTester, Main ( .. ) ) where

import System.Exit
import Data.Data
import Control.Arrow

import System.Console.CmdLib

import PrettyPrint
import Result
import Types
import qualified Testing as T

data Main 
    = CompareTypes { student  :: String
                   , solution :: String
                   }
    | CompareExpressions { student        :: String
                         , solution       :: String
                         , expressionName :: String
                         , limit          :: Maybe Int
                         }
--    | Testfile { student  :: String
--               , testfile :: FilePath
--               }
    deriving ( Typeable, Data, Eq, Show, Read )



instance Attributes Main where

instance RecordCommand Main where
    mode_summary (CompareTypes { }) = "Compare types"
    mode_summary (CompareExpressions { }) = "Compare expressions"
--    mode_summary (Testfile { }) = "Test student's solution using given testfile"

    rec_options (CompareTypes { }) = group "Compare types" [
        student            %> [ Help "student's answer", ArgHelp "TYPE", Required True ],
        solution           %> [ Help "teacher's solution", ArgHelp "TYPE", Required True ]
      ]
    rec_options (CompareExpressions { }) = group "Compare expressions" [
        student            %> [ Help "student's answer", ArgHelp "CODE", Required True ],
        solution           %> [ Help "teacher's solution", ArgHelp "CODE", Required True ],
        expressionName     %> [ Help "name of expression", ArgHelp "NAME", Required True ],
        limit              %> [ Help "time limit, in microseconds"
                              , Default (Nothing :: Maybe Int)
                              , Required False ]
      ]
--    rec_options (Testfile { }) = group "Run testfile" [
--        student            %> [ Help "student's answer", ArgHelp "CODE", Required True ],
--        testfile           %> [ Help "theacher's file with predefined test", ArgHelp "PATH", Required True ]
--      ]

    run' conf _ = runExpressionTester conf >>= \(success, msg) -> do
        putStrLn msg
        if success then exitSuccess else exitWith (ExitFailure 32)

runExpressionTester :: Main -> IO (Bool, String)
runExpressionTester (CompareTypes { student, solution }) =
    fmap formatResult . T.runTest $ T.CompareTypes { student, solution
                                         , typecheckMode = T.RequireTypeOrdering [ Equal ]
                                         , compareMode = T.FullComparison
                                         }

runExpressionTester (CompareExpressions { student, solution, expressionName, limit }) =
    fmap formatResult . T.runTest $
        T.CompareExpressions { limit, expressionName, solution, student
                             , typecheckMode = T.RequireTypeOrdering [ Equal ]
                             , compareMode = T.FullComparison
                             }

-- runExpressionTester (Testfile { student, testfile }) =
--    Testing.runTestfile testfile student >>= return . formatResult

formatResult :: TestResult -> (Bool, String)
formatResult = isSuccess &&& pp

-- | Main function, behaves differently according to the command line arguments
runUI :: [ String ] -> IO ()
runUI = dispatch [] (recordCommands (undefined :: Main))
-- execute (recordCommands (error "a" :: Main))
