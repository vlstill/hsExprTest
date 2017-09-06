{-# LANGUAGE DeriveDataTypeable, CPP #-}

-- | Data type representing result of test and some utility functions.
--
-- * (c) 2012 Martin Jonáš
-- * (c) 2014,2015 Vladimír Štill

module Result ( TestResult(..), isSuccess ) where

import Data.Data
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif
import Text.PrettyPrint

-- | Result of test, success should be tested by 'isSuccess',
-- pattern matchin on 'Success' might be deprecated in future versions.
data TestResult = CompileError { emsg :: String }
                  -- ^ An error occured in compilation phase.
                | TypeError { emsg :: String }
                  -- ^ An error occured in typechcecking phase.
                | RuntimeError { emsg :: String }
                  -- ^ An error encountered while running test this is NOT test
                  -- failure, but unexpected behaviour, such as exception from
                  -- testing functions, or Haskell runtime.
                | Timeout { emsg :: String }
                | TestFailure { emsg :: String }
                  -- ^ Failed testcase or other error reported by test suite.
                | Success -- ^ Test passed.
                deriving ( Eq, Show, Read, Data, Typeable )

-- | Check if test passed.
isSuccess :: TestResult -> Bool
isSuccess Success = True
isSuccess _ = False

-- | 'mappend' is implemented so that it return first unsuccessfull test, 'mempty' is 'Success'.
instance Monoid TestResult where
    mempty = Success
    mappend Success y = y
    mappend x _       = x

-- | Complete result of test, icluding result status (data constructor of
-- 'TestResult' and error message (if present).
instance PrettyPrint TestResult where
    pp Success = "Test passed"
    pp r = "Test failed: " ++ typ ++ ":\n" ++ indent (emsg r)
      where
        typ = show $ toConstr r
        indent = unlines . map ("    " ++) . lines
