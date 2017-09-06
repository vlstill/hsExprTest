{-# LANGUAGE FlexibleContexts, LambdaCase #-}

-- (c) 2017 Vladimír Štill

module Testing.Options ( Options ( Options )
                       , assignment, student, extraFiles, hint, logfile, output
                       , doOut, doLog
                       )
                       where

import Control.Monad.Reader ( reader, MonadReader )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import System.IO ( hPutStrLn, stdout, stderr, withFile, IOMode( AppendMode ), Handle )

data Options = Options { assignment :: FilePath
                       , student    :: FilePath
                       , extraFiles :: [FilePath]
                       , hint       :: Bool
                       , logfile    :: Maybe FilePath
                       , output     :: Maybe FilePath
                       }
    deriving ( Eq, Show, Read )

genericDoOut :: (MonadIO m, MonadReader Options m) => (Options -> Maybe FilePath) -> Handle
                                                   -> String -> m ()
genericDoOut get h str = reader get >>= \case
    Nothing -> liftIO $ hPutStrLn h str
    Just f -> liftIO $ withFile f AppendMode (flip hPutStrLn str)

-- | output logging information (not showed to students)
doLog :: (MonadIO m, MonadReader Options m) => String -> m ()
doLog = genericDoOut logfile stderr

-- | output which will be showed to the student
doOut :: (MonadIO m, MonadReader Options m) => String -> m ()
doOut = genericDoOut output stdout
