-- @ compiled: True

import Test.QuickCheck
import Language.Haskell.TH ( Q, Pat )

expr = "myexp"
timeout = 10
pattern :: Q Pat
pattern = [p| (NonNegative x, NonNegative y) |]

myexp :: Integer -> Integer -> Integer
myexp x y = x ^ y
