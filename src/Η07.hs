module Η07 where

import Zero.Zero
import Control.Arrow

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/07.txt"
   teqt "part 1" 37 $ part1 input
   teqt "part 2" 168 $ part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/07.txt"
   print $ part1 input
   print $ part2 input

parse = parseNums

-- part 1

part1 cs = minimum $ fuel <$> [minimum cs..maximum cs]
   where
   fuel x = sum $ abs . (x-) <$> cs

-- part 2

part2 cs = minimum $ fuel <$> [minimum cs..maximum cs]
   where
   fuel x = sum $ tri . abs . (x-) <$> cs
   tri x = div (x * succ x) 2
