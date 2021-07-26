cross :: Int -> Int -> [(Int, Int)]
cross 0  0  = [(0, 0)]
cross ma mb = [ (a, b) | a <- [0..ma], b <- [0..mb] ]
