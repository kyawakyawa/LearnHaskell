data Tree a = Leaf a | Node (Tree a) (Tree a)

count :: Tree a -> Int
count (Leaf x) = 1
count (Node l r) = count l + count r

balanced :: Tree a -> Bool
balanced (Leaf x) = True
balanced (Node l r) = abs (count l - count r) <= 1 && balanced l && balanced r
