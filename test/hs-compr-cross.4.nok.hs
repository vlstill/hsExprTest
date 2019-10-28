cross :: Int -> Int -> [(Int, Int)]
cross ma mb = concatMap (\a -> map (\b -> (a, b)) [0..mb]) [0..ma]
