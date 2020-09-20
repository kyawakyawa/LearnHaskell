(||) :: Bool -> Bool -> Bool
-- 1
(||) False False = False
(||) _ _ = True

-- 2
-- (||) True _ = True
-- (||) _ y = y

-- 3
-- (||) _ True = True
-- (||) x _ = x

-- 4
-- (||) True True = True
-- (||) True False = True
-- (||) False True = True
-- (||) False False = False

-- 5
-- (||) b c | b == c = b
--          | otherwise = True
