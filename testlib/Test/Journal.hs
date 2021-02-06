{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

{- | A journal can be used (when running under the ExprTest service) to set
points and affect evaluation environment.
-}

module Test.Journal (
          -- * Journal Sinks
          JournalSink (NullSink), getJournalSinkFd,
          JournalMsg (journal),
          -- * Journal Entries
          Points (..),
          -- * Details
          ) where

import System.IO ( Handle, hSetBinaryMode, hSetBuffering, hPutStrLn, BufferMode( LineBuffering ) )
import System.Posix.IO ( fdToHandle )
import System.Posix.Types ( Fd )
import Data.Aeson.TH ( deriveJSON, defaultOptions )
import Data.Aeson.Text ( encodeToLazyText )
import Data.Aeson ( ToJSON, fieldLabelModifier )
import Data.Char ( toLower, isUpper )
import Data.Text.Lazy ( unpack )

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
data Points = Points { points :: Double, outOf :: Double, comment :: String }
              deriving (Show, Eq)


$(let upperToUnderscores :: String -> String
      upperToUnderscores [] = []
      upperToUnderscores (x:xs)
          | isUpper x = '_' : toLower x : upperToUnderscores xs
          | otherwise = x : upperToUnderscores xs
  in deriveJSON defaultOptions { fieldLabelModifier = upperToUnderscores } ''Points)

class ToJSON e => JournalMsg e where
    -- | Submit an entry to journal.
    journal :: JournalSink -> e -> IO ()
    journal NullSink _        = pure ()
    journal (JournalSink h) e = hPutStrLn h . unpack $ encodeToLazyText e

instance JournalMsg Points
