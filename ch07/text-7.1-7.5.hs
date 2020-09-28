-- 7.1
twice :: (a -> a) -> a -> a
twice f x = f (f x)

map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]

-- 7.2

rc_map f (x : xs) = f x : rc_map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs, p x]

rc_filter :: (a -> Bool) -> [a] -> [a]
rc_filter p [] = []
rc_filter p (x : xs)
  | p x = x : rc_filter p xs
  | otherwise = rc_filter p xs

sumsqreven :: [Int] -> Int
sumsqreven ns = Prelude.sum (Main.map (^ 2) (Main.filter even ns))

-- 7.3

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

or :: [Bool] -> Bool
or = foldr (||) True

and :: [Bool] -> Bool
and = foldr (&&) True

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f v [] = v
-- foldr f v (x : xs) = f x (Main.foldr f v xs)

length :: [a] -> Int
length = foldr (\_ n -> 1 + n) 0

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse :: [a] -> [a]
-- reverse [] = []
-- reverse (x : xs) = snoc x (Main.reverse xs)
reverse = foldr snoc []

-- foldr (#) v [x0, x1,...,xn] = x0 # (x1 # (.. (xn # v)...))

-- 7.4

suml :: Num a => [a] -> a
suml = foldl (+) 0

-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldl f v [] = v
-- foldl f v (x : xs) = Main.foldl f (f v x) xs

-- foldl (#) v [x0, x1,..., xn] = (... ((v # x0) # x1) ...) # xn

-- 7.5
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

odd = not Main.. even

twice_ f = f Main.. f

sumsqreven_ = Main.sum Main.. Main.map (^ 2) Main.. Main.filter even

id :: a -> a
id = \x -> x

compose :: [a -> a] -> (a -> a)
compose = foldr (Main..) Main.id