import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

parity_bit :: [Bit] -> Bit
parity_bit bits
  | odd (sum bits) = 1
  | otherwise = 0

make9 :: [Bit] -> [Bit]
make9 bits = make8 bits ++ [parity_bit bits]

encode :: String -> [Bit]
encode = concat . map (make9 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

elim_parity_bit :: [Bit] -> [Bit]
elim_parity_bit bits
  | sum (init bits) `mod` 2 /= last bits = error "error is occured"
  | otherwise = init bits

decode :: [Bit] -> String
decode = map ((chr . bin2int) . elim_parity_bit) . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id
