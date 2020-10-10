putStr :: String -> IO ()
putStr s = sequence_ [putChar c | c <- s]
