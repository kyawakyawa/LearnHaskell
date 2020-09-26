import Data.Char

length :: [a] -> Int
length xs = sum [1 | _ <- xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

lowers :: String -> Int
lowers xs = Prelude.length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = Prelude.length [x' | x' <- xs, x == x']

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table =
  [ 8.1,
    1.5,
    2.8,
    4.2,
    12.7,
    2.2,
    2.0,
    6.1,
    7.0,
    0.2,
    0.8,
    4.0,
    2.4,
    6.7,
    7.5,
    1.9,
    0.1,
    6.0,
    6.3,
    9.0,
    2.8,
    1.0,
    2.4,
    0.2,
    2.0,
    0.1
  ]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a' .. 'z']]
  where
    n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

table' :: [Float]
table' = freqs "kdvnhoo lv ixq"

hoge :: [Float]
hoge = [chisqr (rotate n table') table | n <- [0 .. 25]]

toLowers :: String -> String
toLowers xs = [toLower x | x <- xs]

isUppers :: String -> [Bool]
isUppers xs = [isUpper x | x <- xs]

toUpperIf :: Bool -> Char -> Char
toUpperIf x c
  | x = toUpper c
  | otherwise = c

-- 頻度のリストを回転させながらカイ二乗検定を行う
crack :: String -> String
crack xs = [toUpperIf x c | (x, c) <- zip upper_ids encoded]
  where
    encoded = encode (- factor) xs_
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs_
    upper_ids = [isUpper x | x <- xs]
    xs_ = [toLower x | x <- xs]
