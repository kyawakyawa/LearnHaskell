-- 4.1
even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

recip :: Fractional a => a -> a
recip n = 1 / n

-- 4.2
abs :: Int -> Int
abs n = if n >= 0 then n else - n

signum :: Int -> Int
signum n =
  if n < 0
    then - 1
    else if n == 0 then 0 else 1

-- 4.3
abs_guard :: Int -> Int
abs_guard n
  | n >= 0 = n
  | otherwise = - n

signum_guard :: Int -> Int
signum_guard n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1

-- 4.4
not :: Bool -> Bool
not False = True
not True = False

(&&) :: Bool -> Bool -> Bool
(&&) True b = b
(&&) False b = False
-- (&&) True True = True
-- (&&) _ _ = False
--
-- b && c
--   | b == c = b
--   | otherwise = False

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y


test :: [Char] -> Bool
test ['a',_,_] = True
test _ = False

is_start_a :: [Char] -> Bool
is_start_a ('a' : _) = True
is_start_a _ = False

head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

--4.5

hoge = (\x -> x + x) 2

add :: Int -> Int -> Int
add = \ x -> (\y -> x + y)
-- add = \ x -> \ y -> x + y でも良い

const :: a -> b -> a
-- const x _ = x
const x = \_ -> x

odds :: Int -> [Int]
-- odds n = map f [0..n-1]
--   where f x = x * 2 + 1
odds n = map (\x -> x * 2 + 1) [0..n-1]

hogehoge = (*2) 3 -- 6


sum :: [Int] -> Int
sum = foldl (+) 0
