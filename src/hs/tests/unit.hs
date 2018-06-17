{-# LANGUAGE LambdaCase, TemplateHaskell #-}

-- (c) 2018 Vladimír Štill

import System.Exit ( exitFailure, exitSuccess )
import Test.Expr.TypesUnitTest
import Testlib

main = (fmap and . sequence) $(getTests) >>= \case
          True  -> exitSuccess
          False -> exitFailure
