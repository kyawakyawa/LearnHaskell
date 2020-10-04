--8.5
-- class Eq a where
--   (==), (/=) :: a -> a -> Bool
--   x /= y = not (x == y)
--
-- instance Eq Bool where
--   False == False = True
--   True == True = True
--   _ == _ = False
--
-- class Eq a => Ord a where
--   (<), (<=), (>), (>=) :: a -> a -> Bool
--   min, max             :: a -> a -> a
--   min x y | x <= y = x
--           | otherwise = y
--   max x y | x <= y = y
--           | otherwise = x
--
-- instance Ord Bool where
--   False < True = True
--   _ < _ = False
--
--   b <= c = (b < c)  || (b == c)
--   b > c = c < b
--   b >= c = c <= b
--
-- data Bool = False | True
--             deriving (Eq, Ord, Show, Read)

data Shape = Circle Float | Rect Float Float
  deriving (Eq, Ord)

data Maybe a = Nothing | Just a
  deriving (Eq, Ord)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
-- bools n = map (reverse . map conv . make n . int2bin) range
--   where
--     range = [0..(2^n)-1]
--     make n bs = take n (bs ++ repeat 0)
--     conv 0 = False
--     conv 1 = True
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
  where
    bss = bools (n -1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where
    vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
