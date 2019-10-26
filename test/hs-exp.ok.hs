myexp :: Integer -> Integer -> Integer
myexp x 0 = 1
myexp x y = let e = myexp x (y - 1) in x * ((e + e) `div` 2)
