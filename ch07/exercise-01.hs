hoge f p xs = map f (filter p xs)

hogehoge f p xs = [f x | x <- xs, p x]