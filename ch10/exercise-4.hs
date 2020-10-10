import Data.Char

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  newline
  if isDigit x
    then do return (digitToInt x)
    else do
      putStrLn "ERROR: Invalid digit"
      getDigit prompt

newline :: IO ()
newline = putChar '\n'

adderSub :: Int -> IO Int
adderSub n = do
  if n > 0
    then do
      x <- getDigit ""
      r <- adderSub (n - 1)
      return (x + r)
    else do
      return 0

adder :: IO ()
adder = do
  n <- getDigit "How many numbers? "
  s <- adderSub n
  print ("The total is " ++ (show s))
