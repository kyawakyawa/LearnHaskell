hoge = [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]

hogehoge = concat [[(x, y) | x <- [1, 2, 3]] | y <- [4, 5, 6]]
