-- (c) 2012 Martin Jonáš
-- (c) 2014 Vladimír Štill

module Files (createSolutionFile, createStudentFile) where

import System.IO
import System.Directory
import System.FilePath

-- | Function createCodeFile creates temporary file with given name containing module of given name and with given code.
-- | Marks module as Safe or Unsafe according to the name of the module.
createCodeFile :: String -> String -> String -> IO String
createCodeFile fileName moduleName code = do
    (createdFileName, handle) <- openTempFile "." fileName
    if (moduleName == "Student") then hPutStr handle ("{-# LANGUAGE Safe #-}\n")
                                 else hPutStr handle ("{-# LANGUAGE Unsafe #-}\n")
    hPutStr handle $ unlines [ "{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}"
                             , "module " ++ moduleName ++ " where"
                             , ""
                             , ""
                             , "import Testing.DataTypes"
                             , ""
                             , ""
                             , "{-# LINE 1 \"" ++ moduleName ++ ".hs\" #-}"
                             ]
    hPutStr handle code
    return createdFileName

-- | Function createSolutionFile creates temporary file solution.hs containing module Solution containing given code.
createSolutionFile :: String -> IO String
createSolutionFile f = getTemporaryDirectory >>= \tmp -> createCodeFile (tmp </> "solution.hs") "Solution" f

-- | Function createSolutionFile creates temporary file student.hs containing module Student containing given code.
createStudentFile :: String -> IO String
createStudentFile f = getTemporaryDirectory >>= \tmp -> createCodeFile (tmp </> "student.hs") "Student" f

