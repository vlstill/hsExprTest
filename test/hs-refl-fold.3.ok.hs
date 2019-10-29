lengthFold :: [a] -> Int
lengthFold xs = foldl (\l _ -> l + 1) 0 xs
