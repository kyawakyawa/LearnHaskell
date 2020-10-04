data Expr = Val Int | Add Expr Expr

value :: Expr -> Int
value (Add x y) = value x + value y

hoge = value (Add (Add (Val 2) (Val 3)) (Val 4))

type Cont = [Op]

data Op = EVAL Expr | ADD Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)

value_ :: Expr -> Int
value_ e = eval e []

hogehoge = value_ (Add (Add (Val 2) (Val 3)) (Val 4))
