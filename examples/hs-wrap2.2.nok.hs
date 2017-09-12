inInterval :: Ord a => a -> a -> a -> Bool -- type error: too generic
inInterval x a b = a <= x && x <= b
