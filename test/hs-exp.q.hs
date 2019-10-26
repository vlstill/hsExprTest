-- @ compiled: True

import Test.QuickCheck

expr = "myexp"
timeout = 10
pattern = [p| (NonNegative x, NonNegative y) |]

myexp :: Integer -> Integer -> Integer
myexp x y = x ^ y
