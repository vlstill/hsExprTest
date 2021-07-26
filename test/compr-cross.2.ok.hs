cross :: Int -> Int -> [(Int, Int)]
cross ma mb = [ (a, b) | a <- as, b <- bs ]
  where
    as = [0..ma]
    bs = [0..mb]
