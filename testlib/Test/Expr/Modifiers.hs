-- | A convenience module which exports multiple modifiers from QuickCheck and
-- hsExprTest and the 'TestAs' annotator.

module Test.Expr.Modifiers (
      TestAs
    , module Test.QuickCheck.Modifiers
    , module Test.QuickCheck.Literal
    , module Test.QuickCheck.Range
    , module Test.QuickCheck.Union
    ) where

import Test.QuickCheck.Modifiers
import Test.QuickCheck.Literal
import Test.QuickCheck.Range
import Test.QuickCheck.Union
import Test.Expr.Types ( TestAs )
