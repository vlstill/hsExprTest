{-# LANGUAGE FlexibleContexts #-}

-- | File management utilities.
--
-- * (c) 2012 Martin Jonáš
-- * (c) 2014-2017 Vladimír Štill

module Files (
    -- * File Context
      WorkDir(..)
    , withWorkDir
    , WithWorkDir
    -- * File creation
    , createStudentFile, createSolutionFile, createTestFile
    ) where

import Control.Monad.Reader.Generalized ( GMonadReader, ReaderT, runReaderT, greader )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Catch ( MonadMask )
import System.IO
import System.IO.Temp
import System.FilePath
import Testing.Assignment ( asgnTesterOpts, Assignment )
import Data.List ( find )
import Data.Maybe ( isJust )

-- | 'WorkDir' is a directory in which checker stores all necessary files.
newtype WorkDir = WorkDir { getWorkDir :: FilePath }
                    deriving ( Eq, Show, Read )

type WithWorkDir = ReaderT WorkDir

-- | Create execution context, it will be safely create in temporary directory.
withWorkDir :: (MonadMask m, MonadIO m) => WithWorkDir m a -> m a
withWorkDir act = withSystemTempDirectory "hsExprTestContext" $ \x ->
                  runReaderT act (WorkDir x)

-- | Create temporary file containing given module
--
-- @'withCodeFile' context module content student@ will create file @\<module\>.hs@
-- in given context, file will contain module header and given content, it will
-- be marked either safe or unsafe base on @student@ parameter.
createCodeFile :: (MonadIO m, GMonadReader WorkDir m, GMonadReader Assignment m)
               => String -> String -> Bool -> m FilePath
createCodeFile moduleName content student = do
    fc <- greader getWorkDir
    let name = fc </> moduleName <.> "hs"
    tos <- greader asgnTesterOpts
    let extra = if student && isJust (find (== "RebindableSyntax") tos)
                  then "{-# LANGUAGE RebindableSyntax #-}"
                  else ""
    liftIO . withFile name WriteMode $ \h -> do
        hPutStr h $ unlines
            [ if student then "{-# LANGUAGE Safe, NoTemplateHaskell #-}" else "{-# LANGUAGE Unsafe #-}"
            , extra
            , "module " ++ moduleName ++ " where"
            , ""
            ]
        hPutStr h content
    return name

-- | create file "Student.hs" containing module @Student@ in given context.
-- Module will be marked as safe.
createStudentFile :: (MonadIO m, GMonadReader WorkDir m, GMonadReader Assignment m)
                  => String -> m FilePath
createStudentFile content = createCodeFile "Student" content True

-- | create file "Solution.hs" containing module @Solution@ in given context.
-- Module will be marked as unsafe.
createSolutionFile :: (MonadIO m, GMonadReader WorkDir m, GMonadReader Assignment m)
                   => String -> m FilePath
createSolutionFile content = createCodeFile "Solution" content False

-- | Create test file "Test.hs" which imports student and solution
createTestFile :: (MonadIO m, GMonadReader WorkDir m, GMonadReader Assignment m)
               => String -> m FilePath
createTestFile content0 = createCodeFile "Test" content False
  where
    content = unlines [ "import qualified Student"
                      , "import qualified Solution"
                      ] ++ content0
