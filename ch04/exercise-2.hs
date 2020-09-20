third :: [a] -> a
-- a
third xs = head (tail (tail xs))

-- b
-- third xs = xs !! 2

-- c
-- third (_:_:x:_) = x
