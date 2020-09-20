safetail :: [a] -> [a]
-- a
safetail xs = if length xs == 0 then [] else tail xs

-- b
-- safetail xs | length xs == 0 = []
--             | otherwise tail xs

-- c
-- safetail [] = []
-- safetail (_ : xs) = xs