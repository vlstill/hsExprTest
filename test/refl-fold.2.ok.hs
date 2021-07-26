lengthFold :: [a] -> Int
lengthFold = foldr (\_ l -> 1 + l) 0
