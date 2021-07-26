myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr _ z []     = z
myfoldr f z (x:xs) = x `f` z
