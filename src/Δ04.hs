module Δ04 where

import Zero.Zero
import Prelude hiding ( lookup )
import Data.List ( find )
import Data.List.Split ( splitOn )
import Data.Either ( isRight, isLeft )
import Data.IntMap.Strict as Map ( IntMap, empty, fromList, keys, lookup, insertWith, delete )
import Control.Arrow ( (&&&) )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/04.txt"
   teqt "part 1" 4512 $ part1 input
   teqt "part 2" 1924 $ part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/04.txt"
   print $ part1 input
   print $ part2 input

parse :: String -> ([Int],[Board])
parse = (map read . splitOn "," . head &&& map (Map.fromList . flip zip [0..] . concatMap (map read . words) . lines) . tail) . splitOn "\n\n"

type Board = IntMap Int  -- number => psition

-- part 1

part1 = play . uncurry Stt . fmap (map (Left . (,empty)))

type Marks = IntMap Int  -- line => marks
data Stt = Stt [Int] [Either (Board,Marks) Int]

play :: Stt -> Int
play (Stt [] bms)
   | Just (Right w) <- find isRight bms = w
   | otherwise = error "no win"
play (Stt (n:ns) bms)
   | Just (Right w) <- find isRight bms = w
   | otherwise = play $ Stt ns (map (mark n) bms)

{- rows and columns are indexed in Marks like so:

--   5 6 7 8 9
-- 0 + + + + +
-- 1 + + + + +
-- 2 + + + + +
-- 3 + + + + +
-- 4 + + + + +

-}

mark :: Int -> Either (Board,Marks) Int -> Either (Board,Marks) Int
mark _ (Right w) = Right w
mark n (Left (b,m))
   | Nothing <- i = Left (b,m)
   | win = Right $ n * sum (keys b')
   | otherwise = Left (b',m')
   where
   i = lookup n b
   Just i' = i
   row = div i' 5
   col = mod i' 5 + 5
   b' = delete n b
   m' = insertWith (+) row 1 . insertWith (+) col 1 $ m
   win = lookup row m' == Just 5 || lookup col m' == Just 5

-- part 2

part2 = last . play' . uncurry Stt . fmap (map (Left . (,empty)))

play' :: Stt -> [Int]
play' (Stt [] bms)
   | Just (Right w) <- find isRight bms = [w]
   | otherwise = []
play' (Stt (n:ns) bms)
   | Just (Right w) <- find isRight bms = w : play' (Stt ns (mark n <$> filter isLeft bms))
   | otherwise = play' $ Stt ns (mark n <$> bms)

