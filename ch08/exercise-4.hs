data Tree a = Leaf a | Node (Tree a) (Tree a)

split :: [a] -> ([a], [a])
split xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance (fst sp)) (balance (snd sp))
  where
    sp = split xs
