myexp :: Integer -> Integer -> Integer
myexp x 0 = 1
myexp x y = x * ((myexp x (y - 1) + myexp x (y - 1)) `div` 2)

