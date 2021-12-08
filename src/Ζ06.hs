module Ζ06 where

import Zero.Zero
import Data.IntMap.Strict as Map ( IntMap, insertWith, fromList, elems, (!) )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/06.txt"
   teqt "part 1" 5934 $ part1 input
   teqt "part 2" 26984457539 $ part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/06.txt"
   print $ part1 input
   print $ part2 input

parse = parseNums

-- part 1

part1 = length . (!!80) . iterate day

type Fish = Int
type School = [Fish]

day :: School -> School
day = concatMap go
   where
   go :: Fish -> [Fish]
   go 0 = [6,8]
   go x = [pred x]

-- part 2

part2 :: School -> Integer
part2 = sum . elems . snd . (!!256) . iterate step . stat

-- One singly linked list stores all of
-- the fish in each quantity, where the
-- index is offset by a single integral
-- storing all 9 possible distances. As
-- we iterate through each day the bulk
-- of the update is a linear key map so
-- signified by that single value. Then
-- only the 6's are created, as every 0
-- effectively overflows to an 8. Nice!

type Stat = (D,IntMap Integer)

data D = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8
   deriving ( Enum )

next :: D -> D
next D8 = D0
next d  = succ d

-- build the list

stat :: School -> Stat
stat = (D0,) . flip go (fromList $ zip [0..8] (repeat 0))
   where
   go [] = id
   go (x:xs) = go xs . insertWith (+) x 1

step :: Stat -> Stat
step (d,m) = (d',m')
   where
   d' = next d
   m' = insertWith (+) (ix d' 6) (m ! ix d' 8) m
   --              create the 6's

ix :: D -> Int -> Int
ix d i = mod (fromEnum d + i) 9

