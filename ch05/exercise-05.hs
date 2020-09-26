pyths :: Int -> [(Int, Int, Int)]
pyths l = [(x, y, z) | x <- [1 .. l], y <- [1 .. l], z <- [1 .. l], x ^ 2 + y ^ 2 == z ^ 2]
