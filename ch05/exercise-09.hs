scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | x <- xs, y <- ys]
