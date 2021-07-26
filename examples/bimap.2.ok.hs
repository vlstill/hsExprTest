binmap :: (a -> a -> b) -> [a] -> [b]
binmap f xs = snd $ foldr (\h (mnh, rest) -> (Just h, maybe [] (\nh -> f h nh : rest) mnh)) (Nothing, []) xs

