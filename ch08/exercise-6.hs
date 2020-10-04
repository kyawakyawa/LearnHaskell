data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (\_ -> 1) (+)
