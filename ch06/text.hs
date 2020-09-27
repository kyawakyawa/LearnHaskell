-- 6.1
-- fac :: Int -> Int
-- fac n = product [1..n]

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

-- 6.2
product :: Num a => [a] -> a
product [] = 1
product (n : ns) = n * Main.product ns

length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + Main.length xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = Main.reverse xs Prelude.++ [x]

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs Main.++ ys)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs) = insert x (isort xs)

-- 6.3

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : Main.zip xs ys

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_ : xs) = Main.drop (n - 1) xs

-- 6.5
even :: Int -> Bool
even 0 = True
even n = Main.odd (n - 1)

odd :: Int -> Bool
odd 1 = True
odd n = Main.even (n - 1)

evens :: [a] -> [a]
evens [] = []
evens (x : xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_ : xs) = evens xs