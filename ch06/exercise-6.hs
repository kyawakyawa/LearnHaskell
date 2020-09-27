-- a
and :: [Bool] -> Bool
and [] = True
and (x : xs) = x && (Main.and xs)

-- b
concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = x ++ (Main.concat xs)

-- c
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : Main.replicate (n - 1) x

-- d
(!!) :: [a] -> Int -> a
(!!) (x : xs) 0 = x
(!!) (x : xs) n = xs Main.!! (n - 1)

-- e
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem t (x : xs) = t == x || Main.elem t xs