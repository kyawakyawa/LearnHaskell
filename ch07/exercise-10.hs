luhnDouble :: Int -> Int
luhnDouble x = if 2 * x > 9 then 2 * x - 9 else 2 * x

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x : xs) = f x : altMap g f xs

luhn :: [Int] -> Bool
luhn xs = sum (altMap luhnDouble id xs) `mod` 10 == 0