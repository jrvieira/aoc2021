module Π16 where

import Zero.Zero
import Data.Bits
import Data.Map.Strict qualified as M

test :: IO ()
test = do
-- teqt "part 1" [16,12,23,31] $ part1 . parse <$>
--    [ "8A004A801A8002F478"
--    , "620080001611562C8802118E34"
--    , "C0015000016115A2E0802F182340"
--    , "A0016C880162017C3686B18A3D4780"
--    ]
   teqt "part 2" [3,54,7,9,1,0,0,1] $ part2 . parse <$>
      [ "C200B40A82"
      , "04005AC33890"
      , "880086C3E88112"
      , "CE00C43D881120"
      , "D8005AC2A8F0"
      , "F600BC2D8F"
      , "9C005AC2F8F0"
      , "9C0141080250320F1802104A08"
      ]

main :: IO ()
main = do
   input <- parse . init <$> readFile "./input/16.txt"
   print $ part1 input
   print $ part2 input

type Bin = [Bool]

parse :: String -> [Packet]
parse = packets . concatMap hex
   where
   hex :: Char -> Bin
   hex x = testBit (m M.! x) <$> [3,2,1,0]
   m = M.fromList $ zip (['0'..'9'] <> ['A'..'F']) [0 :: Int ..]

data Packet = Literal Integer [Bin] | Operation Integer Integer [Packet]

instance Show Packet where
   show (Literal _ b) = unwords [show (val $ concat b)]
   show (Operation _ t ps) = "(" <> unwords [o,unwords $ map show ps] <> ")"
      where
      o
         | 0 <- t = "+"
         | 1 <- t = "*"
         | 2 <- t = "m"
         | 3 <- t = "M"
         | 5 <- t = ">"
         | 6 <- t = "<"
         | 7 <- t = "="
         | otherwise = "?"

packets :: Bin -> [Packet]
packets = go Nothing
   where
   go :: Maybe Packet -> Bin -> [Packet]
   go _ [] = []
   go Nothing s
      | null s' = []
      | 4 <- val typ = go (Just (Literal (val ver) [])) s'
      | head s'
      , (op,rest) <- splitAt (fromIntegral $ val len) (go Nothing s'')
      = Operation (val ver) (val typ) op : rest
      | (op,rest) <- splitAt (fromIntegral $ val len) s''
      = Operation (val ver) (val typ) (go Nothing op) : go Nothing rest
      where
      (ini,s') = splitAt 6 s
      (ver,typ) = splitAt 3 ini
      (len,s'') = splitAt (if head s' then 11 else 15) (tail s')
   go (Just (Literal v cs)) s
      | head chnk = go (Just (Literal v cs')) s'
      | otherwise = Literal v (reverse cs') : go Nothing s'
      where
      (chnk,s') = splitAt 5 s
      cs' = tail chnk : cs
   go _ _ = error "illegal construction"

val :: Bin -> Integer
val s = sum [ 2^n | (n,b) <- zip [0..] (reverse s) , b ]

-- part 1

part1 = sum . map vers

vers :: Packet -> Integer
vers (Literal v _) = v
vers (Operation v _ ps) = v + (sum . map vers $ ps)

-- part 2

part2 x = head (map eval x)  # show (head x)

eval :: Packet -> Integer
eval (Literal _ n) = val (concat n)
eval (Operation _ t ps)
   | 0 <- t = sum . map eval $ ps
   | 1 <- t = product . map eval $ ps
   | 2 <- t = minimum . map eval $ ps
   | 3 <- t = maximum . map eval $ ps
   | 5 <- t = if eval p0 > eval p1 then 1 else 0
   | 6 <- t = if eval p0 < eval p1 then 1 else 0
   | 7 <- t = if eval p0 == eval p1 then 1 else 0
   | otherwise = error "illegal op type"
   where
   [p0,p1] = ps
