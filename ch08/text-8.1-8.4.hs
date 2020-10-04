-- 8.1
-- type String = [Char]

type Pos = (Int, Int)

type Trans = Pos -> Pos

-- wrong
-- type Tree = (Int, [Tree])

type Pair a = (a, a)

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

-- 8.2
data Bool = False | True

data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move East (x, y) = (x + 1, y)
move West (x, y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m : ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Main.Maybe Int
safediv _ 0 = Main.Nothing
safediv m n = Main.Just (m `div` n)

safehead :: [a] -> Main.Maybe a
safehead [] = Main.Nothing
safehead xs = Main.Just (head xs)

-- 8.3
-- newtype Nat =  N Int
-- type Nat = Int
-- data Nat = N Int

-- 8.4
data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

add :: Nat -> Nat -> Nat
-- add m n = int2nat (nat2int m + nat2int n)

add Zero n = n
add (Succ m) n = Succ (add m n)

data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Prelude.Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

hoge = flatten t

occurs_ :: Ord a => a -> Tree a -> Prelude.Bool
occurs_ x (Leaf y) = x == y
occurs_ x (Node l y r)
  | x == y = Prelude.True
  | x < y = occurs_ x l
  | otherwise = occurs_ x r

data Tree_ a = Leaf_ a | Node_ (Tree_ a) (Tree_ a)

data Tree__ a = Leaf__ a | Node__ (Tree__ a) (Tree__ a)

data Tree___ a b = Leaf___ a | Node___ (Tree___ a b) b (Tree___ a b)

data Tree____ a = Node____ a [Tree____ a]
