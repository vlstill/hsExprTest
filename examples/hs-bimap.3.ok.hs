binmap :: (a -> a -> b) -> [a] -> [b]
binmap f xs = zipWith f xs (drop 1 xs)

