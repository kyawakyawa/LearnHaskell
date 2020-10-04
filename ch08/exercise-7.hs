data Maybe a = Nothing | Just a

instance Eq a => Eq (Main.Maybe a) where
  Main.Nothing == Main.Nothing = True
  Main.Nothing == Main.Just _ = False
  Main.Just _ == Main.Nothing = False
  Main.Just x == Main.Just y = x == y

data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

zip :: Main.List a -> Main.List b -> Main.List (a, b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (Cons x (xs)) (Cons y (ys)) = Cons (x, y) (Main.zip xs ys)

toPreludeList :: List a -> [a]
toPreludeList Nil = []
toPreludeList (Cons x xs) = x : (toPreludeList xs)

instance Eq a => Eq (List a) where
  x == y = (len x == len y) && and [a == b | (a, b) <- toPreludeList (Main.zip x y)]
