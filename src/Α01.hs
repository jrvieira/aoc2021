module Α01 where

import Zero.Zero
import Data.List
import Control.Arrow ( (&&&) )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/01.txt"
   teqt "part 1" 7 $ part1 input
   teqt "part 2" 5 $ part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/01.txt"
   print $ part1 input
   print $ part2 input

parse :: String -> [Int]
parse = map read . lines

-- part 1

part1 = length . filter id . uncurry (zipWith (<)) . (id &&& tail)

-- part 2

part2 = part1 . map (sum . take 3) . tails

{- more performant alternative

part2 = length . filter id . uncurry (zipWith (<)) . (id &&& drop 3)

-}
