luhnDouble :: Int -> Int
luhnDouble x = if 2 * x > 9 then 2 * x - 9 else 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z w = (luhnDouble x + y + luhnDouble z + w) `mod` 10 == 0
