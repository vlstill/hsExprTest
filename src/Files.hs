-- (c) 2012 Martin Jonáš
-- (c) 2014,2015 Vladimír Štill

module Files (
    -- * File Context
      FileContext(..)
    , withContext
    -- * File creation
    , createCodeFile, createStudentFile, createSolutionFile
    ) where

import System.IO
import System.IO.Temp
import System.FilePath

newtype FileContext = FileContext { getContext :: FilePath }
                    deriving ( Eq, Show, Read )


-- | Create execution context, which is actually a directory in which all files are held
withContext :: (FileContext -> IO a) -> IO a
withContext yield = withSystemTempDirectory "hsExprTestContext" $ yield . FileContext

-- | Create temporary file with containing given module
--
-- @withCodeFile context module content safe@ will create file @<module>.hs@
-- in given context, file will contain module header and given context, it will
-- be marked either safe or unsafe base on @safe@ parameter.
createCodeFile :: FileContext -> String -> String -> Bool -> IO FilePath
createCodeFile (FileContext fc) moduleName content safe = do
    let name = fc </> moduleName <.> "hs"
    withFile name WriteMode $ \h -> do
        hPutStr h $ unlines
            [ if safe then "{-# LANGUAGE Safe #-}" else "{-# LANGUAGE Unsafe #-}"
            , "module " ++ moduleName ++ " where"
            , ""
            , "{-# LINE 1 \"" ++ moduleName ++ ".hs\" #-}"
            ]
        hPutStr h content
    return name

-- | create file "Student.hs" containing module @Student@ in given context.
-- Module will be marked as safe.
createStudentFile :: FileContext -> String -> IO FilePath
createStudentFile fc content = createCodeFile fc "Student" content True

-- | create file "Solution.hs" containing module @Solution@ in given context.
-- Module will be marked as unsafe.
createSolutionFile :: FileContext -> String -> IO FilePath
createSolutionFile fc content = createCodeFile fc "Solution" content False

