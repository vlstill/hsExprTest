{-# LANGUAGE LambdaCase #-}

-- (c) 2018 Vladimír Štill

import System.Exit
import Test.Expr.TypesUnitTest

main = (fmap and . sequence) [test_types] >>= \case
          True  -> exitSuccess
          False -> exitFailure
