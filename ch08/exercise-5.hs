data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x

foldl f g (Add x y) = g (folde f g x) (folde f g y)
