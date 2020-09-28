dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x * 10 + y) 0