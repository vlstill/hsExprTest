-- @ expr: binmap
-- @ limit: 5

binmap :: (a -> a -> b) -> [a] -> [b]
binmap _ []       = []
binmap _ [_]      = []
binmap f (x:y:xs) = f x y : binmap f (y:xs)
