import Test.QuickCheck.Modifiers ( NonNegative ( NonNegative ) )

expr = "f"
pattern = [p| (NonNegative x) |]

f :: Int -> Int
f x = x
