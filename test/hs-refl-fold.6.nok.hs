lengthFold :: [a] -> Int
lengthFold = foldl (\l _ -> l + 1) 0 . id . id
