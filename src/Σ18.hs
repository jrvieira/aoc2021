module Σ18 where

import Zero.Zero

import Data.Word
import Data.List.Split ( splitOn )
import Data.Char ( isDigit )
import Data.Tuple ( swap )
import Data.Sequence ( Seq(..), (|>), (<|), (><) )
import Data.Bifunctor ( first, second )
import Control.Arrow ( (&&&) )

test :: IO ()
test = do
   input <- map parse . splitOn "\n\n" <$> readFile "./tests/18.txt"
   teqt "part 1" 4140 $ part1 (last input)
   teqt "part 2" 3993 $ part2 (last input)

-- mapM_ (uncurry $ teqt "magnitude") (fmap (magnitude . reduce . snum) <$> [
--    (143,"[[1,2],[[3,4],5]]") ,
--    (1384,"[[[[0,7],4],[[7,8],[6,0]]],[8,1]]") ,
--    (445,"[[[[1,1],[2,2]],[3,3]],[4,4]]") ,
--    (791,"[[[[3,0],[5,3]],[4,4]],[5,5]]") ,
--    (1137,"[[[[5,0],[7,4]],[5,5]],[6,6]]") ,
--    (3488,"[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]") ])

-- mapM_ (uncurry $ teqt "sums") $ zip (snum <$> [
--    "[[[[1,1],[2,2]],[3,3]],[4,4]]" ,
--    "[[[[3,0],[5,3]],[4,4]],[5,5]]" ,
--    "[[[[5,0],[7,4]],[5,5]],[6,6]]" ,
--    "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" ,
--    "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]" ])
--    $ map (foldl1 add) input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/18.txt"
   print $ part1 input
   print $ part2 input

parse :: String -> [Snum]
parse = map snum . lines

type Snum = Seq (Word,Word)
--               ^num ^lvl

snum :: String -> Snum
snum = go Empty 0
   where
   go acc 0 "" = acc -- # unwords ["snum:",show acc]
   go acc lvl s@(c:cs)
      | isDigit c = let (n,r) = span isDigit s in go (acc |> (read n,lvl)) lvl r
      | ',' <- c = go acc lvl cs
      | '[' <- c = go acc (succ lvl) cs
      | ']' <- c = go acc (pred lvl) cs
   go _ _ s = error $ unwords ["parse error:",s]

-- part 1

part1 :: [Snum] -> Word
part1 = magnitude . foldl1 add

add :: Snum -> Snum -> Snum
add a b = reduce $ fmap succ <$> a >< b
-- # unwords ["\n",show $ fst <$> a,"\n",show $ fst <$> b]

reduce :: Snum -> Snum
reduce s
   | any ((> 4) . snd) s = reduce (explode s) -- # "exploding..."
   | any ((> 9) . fst) s = reduce (split s) -- # "splitting..."
   | otherwise = s -- # unwords ["reduced:",show $ fst <$> s]

explode :: Snum -> Snum
explode = go Empty
   where
   go acc Empty = acc
   go acc ((n,l) :<| r)
      | l < 5 = go (acc :|> (n,l)) r
      | otherwise = go acc' r'
      where
      acc'
         | Empty <- acc = Empty |> (0,pred l)
         | s :|> x <- acc = s |> first (+ n) x |> (0,pred l)
      r'
         | (_,5) :<| Empty <- r = Empty
         | (n',5) :<| (m,k) :<| s <- r = (n'+m,k) <| s
         | otherwise = error $ unwords ["can't explode r:",show r]

split :: Snum -> Snum
split = go Empty
   where
   go acc Empty = acc
   go acc ((n,l) :<| r)
      | n < 10 = go (acc :|> (n,l)) r
      | otherwise = acc >< (nl,succ l) <| (nr,succ l) <| r
      where
      (nl,nr) = fst &&& uncurry (+) $ quotRem n 2

data Tree = Num Word | Pair Tree Tree

magnitude :: Snum -> Word
magnitude s = go depth Empty s
   where
   depth = maximum (snd <$> s)
   go :: Word -> Seq (Word, Word) -> Seq (Word, Word) -> Word
-- go l' s' s | False  # show (snd <$> s') = undefined
   go 0 Empty ((n,0) :<| Empty) = n -- # unwords ["done"]
   go l' s' Empty = go (pred l') Empty s'
   go l' s' s@((n,l) :<| r)
      | l' == l , (m,k) :<| r' <- r , l == k = go l' (s' |> (n*3+m*2,pred l)) r'
      | otherwise = go l' (s' |> (n,l)) r

-- part 2

part2 :: [Snum] -> Word
part2 = maximum . map (magnitude . uncurry add) . pairs
   where
   pairs :: [Snum] -> [(Snum,Snum)]
   pairs ls = [ (a,b) | a <- ls , b <- ls , a /= b ]
