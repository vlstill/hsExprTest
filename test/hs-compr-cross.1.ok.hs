cross :: Int -> Int -> [(Int, Int)]
cross ma mb = [ (a, b) | a <- [0..ma], b <- [0..mb] ]
