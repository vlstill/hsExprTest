-- (c) 2012 Martin Jonáš
-- (c) 2014 Vladimír Štill

module Files (createSolutionFile, createStudentFile) where

import System.IO
import System.Directory

-- | Function createCodeFile creates temporary file with given name containing module of given name and with given code.
-- | Marks module as Safe or Unsafe according to the name of the module.
createCodeFile :: String -> String -> String -> IO String
createCodeFile fileName moduleName code = do
    tmp <- getTemporaryDirectory
    (createdFileName, handle) <- openTempFile tmp fileName
    if moduleName == "Student" then hPutStr handle "{-# LANGUAGE Safe #-}\n"
                               else hPutStr handle "{-# LANGUAGE Unsafe #-}\n"
    hPutStr handle $ unlines [ "{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}"
                             , "module " ++ moduleName ++ " where"
                             , ""
                             , "{-# LINE 1 \"" ++ moduleName ++ ".hs\" #-}"
                             ]
    hPutStr handle code
    return createdFileName

-- | Function createSolutionFile creates temporary file solution.hs containing module Solution containing given code.
createSolutionFile :: String -> IO String
createSolutionFile = createCodeFile "solution.hs" "Solution"

-- | Function createSolutionFile creates temporary file student.hs containing module Student containing given code.
createStudentFile :: String -> IO String
createStudentFile = createCodeFile "student.hs" "Student"

