binmap :: (a -> a -> b) -> [a] -> [b]
binmap f (x:y:xs) = repeat (f x y)
binmap _ _ = []
