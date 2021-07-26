inInterval :: Ord a => a -> a -> a -> Bool
inInterval x a b = a <= x && x <= b
