myif :: Bool -> a -> a -> a
myif True  x _ = x
myif False _ y = y
