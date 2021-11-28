module Θ08 where

import Zero.Zero

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/08.txt"
   teqt "part 1" "" $ part1 input
-- teqt "part 2" "" $ part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/08.txt"
   print $ part1 input
-- print $ part2 input

parse = id

-- part 1

part1 = id

-- part 2


