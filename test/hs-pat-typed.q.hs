import Test.QuickCheck.Modifiers ( NonNegative ( NonNegative ) )
import Language.Haskell.TH ( Q, Pat )

expr = "f"
pattern :: Q Pat
pattern = [p| (NonNegative x :: NonNegative Int) |]

f :: Int -> Int
f x = x

