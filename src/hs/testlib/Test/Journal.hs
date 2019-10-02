{-# LANGUAGE NamedFieldPuns #-}

{- | A journal can be used (when running under the ExprTest service) to set
points and affect evaluation environment.
-}

module Test.Journal (
          -- * Journal Sinks
          JournalSink (NullSink), getJournalSinkFd,
          JournalMsg (journal, getJS),
          -- * Journal Entries
          Points (..),
          -- * Details
          ) where

import System.IO ( Handle, hSetBinaryMode, hSetBuffering, hPutStrLn, BufferMode( LineBuffering ) )
import System.Posix.IO ( fdToHandle )
import System.Posix.Types ( Fd )
import Text.JSON

-- | A journal is either 'NullSink' (journalling does nothing) or is a sing
-- created with 'getJournalSinkFd'.
data JournalSink = NullSink
                 | JournalSink Handle

-- | Get a 'JournalSink' from a file descriptor. Journalling to aprropriate
-- file descriptor provided by ExprTest service allows you to control evaluation
-- environment and assing points.
getJournalSinkFd :: Fd -> IO JournalSink
getJournalSinkFd fd = JournalSink <$> do
                        h <- fdToHandle fd
                        hSetBuffering h LineBuffering
                        hSetBinaryMode h False
                        pure h

-- | Used for assigning points.
data Points = Points { points :: Integer, outOf :: Integer, comment :: String }
              deriving (Show, Eq)

class JournalMsg e where
    -- | Submit an entry to journal.
    journal :: JournalSink -> e -> IO ()
    journal NullSink _        = pure ()
    journal (JournalSink h) e = hPutStrLn h . encode $ getJS e

    -- | Implementation of entry serialization.
    getJS :: e -> JSObject JSValue

instance JournalMsg Points where
    getJS Points { points, outOf, comment } = toJSObject
                                                [ ("points", showJSON points)
                                                , ("out_of", showJSON outOf)
                                                , ("comment", showJSON comment)
                                                ]
