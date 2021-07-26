import Test.Expr
import Test.QuickCheck.Modifiers

expr = "f"

f :: Integral a => (a `TestAs` Positive a) -> Bool
f _ = True
