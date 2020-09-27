-- a
sum :: Num a => [a] -> a
sum [] = 0
sum (x : xs) = x + Main.sum xs

-- b
take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x : xs) = x : Main.take (n - 1) xs

-- c
last :: [a] -> a
last [x] = x
last (x : xs) = Main.last xs