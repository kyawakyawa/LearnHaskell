bools :: [Bool]
bools = [True, False]

num :: [[Int]]
num = [[4], []]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply x y = x y