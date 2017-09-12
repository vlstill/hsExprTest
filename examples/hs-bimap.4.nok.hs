binmap :: (a -> a -> b) -> [a] -> [b]
binmap f = drop 1 . map (\x -> f x x)
