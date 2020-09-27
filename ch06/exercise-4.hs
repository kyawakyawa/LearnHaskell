euclid :: Int -> Int -> Int
euclid a b
  | b == 0 = a
  | otherwise = euclid b (a `mod` b)