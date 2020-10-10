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

adder :: IO ()
adder = do
  n <- getDigit "How many numbers? "
  inputs <- sequence [getDigit "" | i <- [1 .. n]]
  print (sum inputs)
