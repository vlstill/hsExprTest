expr = "inInterval_wrap"
timeout = 4

-- @ INJECT BEGIN
inInterval_wrap :: Int -> Int -> Int -> Bool
inInterval_wrap x a b = inInterval x (min a b) (max a b)
-- @ INJECT END

inInterval :: Int -> Int -> Int -> Bool
inInterval x a b = a <= x && x <= b

