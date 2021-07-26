expr = "funzip"

funzip :: [a -> b] -> [a] -> [b]
funzip = zipWith id
