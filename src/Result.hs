{-# LANGUAGE DeriveDataTypeable #-}

-- (c) 2012 Martin Jonáš
-- (c) 2014,2015 Vladimír Štill

module Result ( TestResult(..), isSuccess ) where

import Data.Monoid
import Data.Typeable ( Typeable )
import Data.Data
import PrettyPrint
import Data.List

data TestResult = CompileError { emsg :: String }
                  -- ^ error occured in compilation phase
                | TypeError { emsg :: String }
                  -- ^ error occured in typechcecking phase
                | RuntimeError { emsg :: String }
                  -- ^ an error encountered while running test this is NOT test
                  -- failure, but unexpected behaviour, such as exception from
                  -- testing functions
                | Timeout { emsg :: String }
                | TestFailure { emsg :: String }
                  -- ^ failed testcase or other error reported by test suite
                | Success -- ^ test passed
                deriving ( Eq, Show, Read, Data, Typeable )

isSuccess :: TestResult -> Bool
isSuccess Success = True
isSuccess _ = False

instance Monoid TestResult where
    mempty = Success
    mappend Success y = y
    mappend x _       = x

instance PrettyPrint TestResult where
    pp Success = "Test passed"
    pp r = "Test failed: " ++ typ ++ ":\n" ++ indent (emsg r)
      where
        typ = show $ toConstr r
        indent = unlines . map ("    " ++) . lines
