module Ξ14 where

import Zero.Zero ( index, teqt )
import Data.Map.Strict ( Map, singleton, fromList, mapWithKey, unionsWith, unionWith, (!?) )
import Control.Arrow ( first, second, (&&&) )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/14.txt"
   teqt "part 1" 1588 $ part1 input
   teqt "part 2" 2188189693529 $ part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/14.txt"
   print $ part1 input
   print $ part2 input

parse = first head . second (fromList . fmap (((head &&& last) . head &&& head . last) . words) . tail) . break (== "") . lines

-- part 1

part1 (pt,rs) = calc $ index $ (!!10) $ iterate go pt
   where
   go :: String -> String
   go (a:b:xs)
      | Just c <- rs !? (a,b) = a : c : go (b:xs)
      | otherwise = a : go (b:xs)
   go xs = xs

calc = uncurry (-) . (maximum &&& minimum)

-- part 2

type Pair = (Char,Char)

part2 (pt,rs) = calc $ unionsWith (+) $ index pt : (insertions 40 <$> pairs pt)
   where
   insertions :: Int -> Pair -> Map Char Integer
   insertions n p = maybe mempty (unionsWith (+) . take n) (mem !? p)
   -- memoization
   mem :: Map Pair [Map Char Integer]
   mem = mapWithKey expand rs
   expand :: Pair -> Char -> [Map Char Integer]
   expand (a,b) c = singleton c 1 : zipWith (unionWith (+)) (go (a,c)) (go (c,b))
   go :: Pair -> [Map Char Integer]
   go p = maybe (repeat mempty) id (mem !? p)

pairs :: String -> [Pair]
pairs (a:b:xs) = (a,b) : pairs (b:xs)
pairs _ = []

