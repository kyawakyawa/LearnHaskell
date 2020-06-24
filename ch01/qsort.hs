qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, b > x]
qsort_descending_order [] = []
qsort_descending_order (x:xs) = qsort_descending_order larger ++ [x] ++ qsort_descending_order smaller
                                where
                                  larger  = [a | a <- xs, a >= x]
                                  smaller = [b | b <- xs, b < x]
