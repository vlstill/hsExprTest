myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr _ z [] = z
myfoldr f z xs@(x:_) = x `f` myfoldr f z xs
