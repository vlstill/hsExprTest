{-# LANGUAGE FlexibleContexts, LambdaCase #-}

-- (c) 2017 Vladimír Štill

module Testing.Options ( Options ( Options )
                       , optAssignment, optStudent, optExtraFiles, optHint, optIncludeDirs
                       , doOut, doLog
                       , WithOptions
                       , withOptions
                       )
                       where

import Control.Monad.Reader.Generalized ( ReaderT, runReaderT )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import System.IO ( hPutStrLn, stdout, stderr, Handle )
import Data.Time.Format ( formatTime, defaultTimeLocale )
import Data.Time.LocalTime ( getZonedTime )
import Data.List ( intercalate )
import Data.Semigroup ( Semigroup, Monoid, mappend, mempty, (<>) )

data Options = Options { optAssignment :: FilePath
                       , optStudent    :: FilePath
                       , optExtraFiles :: [FilePath]
                       , optIncludeDirs :: [FilePath]
                       , optHint       :: Bool
                       }
    deriving ( Eq, Show )

instance Semigroup Options where
    o1 <> o2 = Options { optAssignment = query optAssignment
                       , optStudent = query optStudent
                       , optExtraFiles = optExtraFiles o1 <> optExtraFiles o2
                       , optIncludeDirs = optIncludeDirs o1 <> optIncludeDirs o2
                       , optHint = optHint o1 || optHint o2
                       }
      where
        query get = if null (get o1) then get o2 else get o1

instance Monoid Options where
    mempty = Options { optAssignment = ""
                     , optStudent = ""
                     , optExtraFiles = []
                     , optIncludeDirs = []
                     , optHint = False
                     }
    mappend = (<>)

type WithOptions = ReaderT Options

withOptions :: MonadIO m => Options -> WithOptions m a -> m a
withOptions opts act = runReaderT act opts

genericDoOut :: MonadIO m => Handle -> String -> m ()
genericDoOut h str = liftIO $ hPutStrLn h str

-- | output logging information (not showed to students)
doLog :: MonadIO m => String -> m ()
doLog msg = liftIO stamp >>= \s -> genericDoOut stderr (prefixed s)
  where
    stamp :: IO String
    stamp = formatTime defaultTimeLocale "[%d-%m-%Y %T]" <$> getZonedTime
    prefixed s = intercalate "\n" . map (\x -> s ++ " " ++ x) $ lines msg

-- | output which will be showed to the student
doOut :: MonadIO m => String -> m ()
doOut = genericDoOut stdout

