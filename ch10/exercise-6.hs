import System.IO

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

readLineSub :: IO (String, Int)
readLineSub = do
  x <- getCh
  case x of
    '\n' -> do
      putChar x
      return ([], 0)
    '\DEL' -> do
      putChar '\b'
      putChar ' '
      putChar '\b'
      (xs, n) <- readLineSub
      return (xs, n + 1)
    otherwise -> do
      putChar x
      (xs, n) <- readLineSub
      if n == 0
        then do
          return ((x : xs), 0)
        else do
          return (xs, n - 1)

readLine :: IO String
readLine = do
  (xs, n) <- readLineSub
  return xs