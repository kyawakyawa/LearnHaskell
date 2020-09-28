all :: (a -> Bool) -> [a] -> Bool
all f = foldr (\x y -> f x && y) True

any :: (a -> Bool) -> [a] -> Bool
any f = foldr (\x y -> f x || y) False

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x : xs)
  | f x = x : Main.takeWhile f xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (x : xs)
  | f x = Main.dropWhile f xs
  | otherwise = (x : xs)