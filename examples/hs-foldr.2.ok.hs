myfoldr _ z []     = z
myfoldr f z (x:xs) = x `f` myfoldr f z xs
