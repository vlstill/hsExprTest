lengthFold :: [a] -> Int
lengthFold xs = foldr (\_ l -> 1 + l) 0 xs
