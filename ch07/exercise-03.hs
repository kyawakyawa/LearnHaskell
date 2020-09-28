map :: (a -> b) -> [a] -> [b]
map f = foldr (\x y -> f x : y) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x y -> if f x then x : y else y) []