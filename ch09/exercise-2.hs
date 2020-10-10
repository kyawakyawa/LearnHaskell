erase_first_of :: Eq a => a -> [a] -> [a]
erase_first_of _ [] = []
erase_first_of p (x : xs)
  | p == x = xs
  | otherwise = x : erase_first_of p xs

erase :: Eq a => [a] -> [a] -> [a]
erase [] _ = []
erase xs [] = xs
erase xs (y : ys) = erase (erase_first_of y xs) ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice xs ys = erase xs ys == []
