myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr _ z [] = z
myfoldr f z xs = myfoldr f z xs

