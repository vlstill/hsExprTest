funzip :: [a -> b] -> [a] -> [b]
funzip = zipWith ($)
