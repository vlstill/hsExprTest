import Test.QuickCheck.Modifiers ( NonNegative ( NonNegative ) )

expr = "f"
pattern = [p| (NonNegative x :: NonNegative Int) |]

f :: Int -> Int
f x = x

