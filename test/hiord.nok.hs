f :: (a -> a -> a) -> a -> [a] -> a
f = foldr
