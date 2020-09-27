length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + Main.length xs

-- length [1, 2, 3]
-- = {lengthを適用}
--    1 + length [2, 3]
-- = {lengthを適用}
--    1 + (1 + length [3])
-- = {lengthを適用}
--    1 + (1 + (1 + length []))
-- = {lengthを適用}
--    1 + (1 + (1 + 0))
-- = {+を適用}
--    3

drop :: Integral b => b -> [a] -> [a]
drop 0 xs = xs
drop n [] = []
drop n (x : xs) = Main.drop (n - 1) xs

-- drop 3 [1, 2 ,3, 4, 5]
-- = {dropを適用}
--    drop 2 [2, 3, 4, 5]
-- = {dropを適用}
--    drop 1 [3, 4, 5]
-- = {dropを適用}
--    drop 0 [4, 5]
-- = {dropを適用}
--    [4, 5]

init :: [a] -> [a]
init [_] = []
init (x : xs) = x : Main.init xs

-- init [1, 2, 3]
-- = {initを適用}
--    1 : init [2, 3]
-- = {initを適用}
--    1 : (2 : init [3])
-- = {initを適用}
--    1 : (2 : [])
-- = {:を適用}
--    [1, 2]