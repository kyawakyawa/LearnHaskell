grid :: Int -> Int -> [(Int, Int)]
grid x y = [(x', y') | x' <- [0 .. x], y' <- [0 .. y]]

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]
