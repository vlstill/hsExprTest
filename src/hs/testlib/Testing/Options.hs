{-# LANGUAGE FlexibleContexts, LambdaCase #-}

-- (c) 2017 Vladimír Å till

module Testing.Options ( Options ( Options )
                       , optAssignment, optStudent, optExtraFiles, optHint, optLogFile, optOutFile
                       , doOut, doLog
                       , WithOptions
                       , withOptions
                       )
                       where

import Control.Monad.Reader.Generalized ( greader, GMonadReader, ReaderT, runReaderT )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import System.IO ( hPutStrLn, stdout, stderr, withFile, IOMode( AppendMode ), Handle )
import Data.Time.Format ( formatTime, defaultTimeLocale )
import Data.Time.LocalTime ( getZonedTime )
import Data.List ( intercalate )
import Data.Monoid ( Monoid, mappend, mempty, (<>), getFirst, First ( First ) )

data Options = Options { optAssignment :: FilePath
                       , optStudent    :: FilePath
                       , optExtraFiles :: [FilePath]
                       , optHint       :: Bool
                       , optLogFile    :: Maybe FilePath
                       , optOutFile    :: Maybe FilePath
                       }
    deriving ( Eq, Show, Read )

instance Monoid Options where
    mempty = Options { optAssignment = ""
                     , optStudent = ""
                     , optExtraFiles = []
                     , optHint = False
                     , optLogFile = Nothing
                     , optOutFile = Nothing }
    mappend o1 o2 = Options { optAssignment = query optAssignment
                            , optStudent = query optStudent
                            , optExtraFiles = optExtraFiles o1 <> optExtraFiles o2
                            , optHint = optHint o1 || optHint o2
                            , optLogFile = getFirst $ First (optLogFile o1) <> First (optLogFile o2)
                            , optOutFile = getFirst $ First (optOutFile o1) <> First (optOutFile o2)
                            }
      where
        query get = if null (get o1) then get o2 else get o1

type WithOptions = ReaderT Options

withOptions :: MonadIO m => Options -> WithOptions m a -> m a
withOptions opts act = runReaderT act opts

genericDoOut :: (MonadIO m, GMonadReader Options m) => (Options -> Maybe FilePath) -> Handle
                                                   -> String -> m ()
genericDoOut get h str = greader get >>= \case
    Nothing -> liftIO $ hPutStrLn h str
    Just f -> liftIO $ withFile f AppendMode (flip hPutStrLn str)

-- | output logging information (not showed to students)
doLog :: (MonadIO m, GMonadReader Options m) => String -> m ()
doLog msg = liftIO stamp >>= \s -> genericDoOut optLogFile stderr (prefixed s)
  where
    stamp :: IO String
    stamp = formatTime defaultTimeLocale "[%d-%m-%Y %T]" <$> getZonedTime
    prefixed s = intercalate "\n" . map (\x -> s ++ " " ++ x) $ lines msg

-- | output which will be showed to the student
doOut :: (MonadIO m, GMonadReader Options m) => String -> m ()
doOut = genericDoOut optOutFile stdout
