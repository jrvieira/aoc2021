module Θ08 where

import Zero.Zero
import Data.List
import Data.IntMap ( IntMap )
import qualified Data.IntMap.Strict as M
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Bifunctor
import Data.Char
import Data.Tuple
import Control.Monad

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/08.txt"
   teqt "part 1" 26 $ part1 input
   teqt "part 2" 61229 $ part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/08.txt"
   print $ part1 input
   print $ part2 input

parse :: String -> [Entry]
parse = map (join bimap (map S.fromList) . fmap tail . break (== "|") . words) . lines

-- part 1

part1 :: [Entry] -> Int
part1 = length . concatMap (filter ((∈ [2,3,4,7]) . length) . snd)

type Segment = Char
type Digit = Set Segment
type Entry = ([Digit],[Digit])

-- part 2

part2 :: [Entry] -> Int
part2 = sum . map (read . map intToDigit . deduce)

-- deduce correct digit by its unique signature

deduce :: Entry -> [Int]
deduce (ds,r) = map (signatures M.!) sigs
   where
   sigs :: [Int]
   sigs = sum . map (frequency ds) . S.toList <$> r

-- for each digit, the sum of segment frequencies in [0..9] is unique

signatures :: IntMap Int
signatures = M.fromList $ map swap $ M.assocs sigs
   where
   sigs = M.map (sum . map (frequency $ M.elems digits) . S.toList) digits

-- number of times each segment appears in [0..9]

frequency :: [Digit] -> Segment -> Int
frequency ds s = frequencies ds Map.! s

frequencies :: [Digit] -> Map Segment Int
frequencies = foldl' count (Map.fromList $ zip "abcdefg" [0,0..])
   where
   count :: Map Segment Int -> Digit -> Map Segment Int
   count = S.foldl' (flip $ Map.adjust succ)

-- map correct segments

digits :: IntMap Digit
digits = M.fromList [
   (0,S.fromList "abcefg" ),
   (1,S.fromList "cf"     ),
   (2,S.fromList "acdeg"  ),
   (3,S.fromList "acdfg"  ),
   (4,S.fromList "bcdf"   ),
   (5,S.fromList "abdfg"  ),
   (6,S.fromList "abdefg" ),
   (7,S.fromList "acf"    ),
   (8,S.fromList "abcdefg"),
   (9,S.fromList "abcdfg" )]

{- Study of possible unique invariants:

   0  1  2  3  4  5  6  7  8  9

   a     a  a     a  a  a  a  a   8  -  6 55 56376
   b           b  b  b     b  b   6  -  6   456 76
   c  c  c  c  c        c  c  c   8  -  62554  376
         d  d  d  d  d     d  d   7  -    55456 76
   e     e           e     e      4  -  6 5   6 7
   f  f     f  f  f  f  f  f  f   9  -  62 5456376
   g     g  g     g  g     g  g   7  -  6 55 56 76

   6  2  5  5  4  5  6  3  7  6

   -  -  -  -  -  -  -  -  -  -

   8     8  8     8  8  8  8  8
   6        6  6  6  6     6  6
   8  8  8  8  8        8  8  8
         7  7  7  7  7     7  7
   4     4           4     4
   9  9     9  9  9  9  9  9  9
   7     7  7     7  7     7  7

   =  =  =  =  =  =  =  =  =  =
   42 17 34 39 30 37 41 25 49 45 <= We choose this sum

-}
