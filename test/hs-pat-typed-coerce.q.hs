import Test.QuickCheck.Modifiers ( NonNegative ( NonNegative ) )

expr = "f"
pattern = [p| (x :: NonNegative Int) |]

f :: Int -> Int
f x = x

