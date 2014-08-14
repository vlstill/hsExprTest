module Files (createSolutionFile, createStudentFile) where

import System.IO

-- | Function createCodeFile creates temporary file with given name containing module of given name and with given code.
-- | Marks module as Safe or Unsafe according to the name of the module.
createCodeFile :: String -> String -> String -> IO String
createCodeFile fileName moduleName code = do
    (createdFileName, handle) <- openTempFile "." fileName
    if (moduleName == "Student") then hPutStr handle ("{-# LANGUAGE Safe #-}\n")
                                 else hPutStr handle ("{-# LANGUAGE Unsafe #-}\n")
    hPutStr handle ("module " ++ moduleName ++ " where \n\nimport InteractiveImports.DataTypes \n\n")
    hPutStr handle code
    return createdFileName

-- | Function createSolutionFile creates temporary file solution.hs containing module Solution containing given code.
createSolutionFile :: String -> IO String
createSolutionFile = createCodeFile "solution.hs" "Solution"

-- | Function createSolutionFile creates temporary file student.hs containing module Student containing given code.
createStudentFile :: String -> IO String
createStudentFile = createCodeFile "student.hs" "Student"

