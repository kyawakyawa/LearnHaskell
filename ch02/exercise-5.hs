myinit1 xs = reverse (tail (reverse xs))

myinit2 xs = take (length xs -1) xs