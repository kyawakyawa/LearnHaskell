type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

putBoardSub :: Int -> Board -> IO ()
putBoardSub _ [] = do
  return ()
putBoardSub n (b : bs) = do
  putRow n b
  putBoardSub (n + 1) bs

putBoard :: Board -> IO ()
putBoard = putBoardSub 1
