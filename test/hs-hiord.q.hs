expr = "f"

f :: (a -> a -> a) -> a -> [a] -> a
f = foldl
