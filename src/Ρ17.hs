module Ρ17 where

import Zero.Zero

import Data.List ( lookup )
import Data.Bifunctor ( bimap )
import Data.Maybe ( mapMaybe )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/17.txt"
   teqt "part 1" "" $ part1 input
-- teqt "part 2" "" $ part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/17.txt"
   print $ part1 input
-- print $ part2 input

parse = id

part1 = id

{- axioms:

-- final x ∈ T
-- x = n in Tn <- final x
-- where Tn = triangular number

-}

tn :: Num a => a -> Maybe a
tn x = lookup (abs x) $ takeUntil (> x) triangulars
   where
   triangulars = zip (triangular <$> [0..]) [0..]
   triangular n = div (succ n * n) 2

possibleTargetsX :: Num a => [a] -> [a]
possibleTargetsX = mapMaybe tn

step (x,y) = bimap (drag . (+ x)) (gravity . (+ y)) (x,y)
   where
   gravity = pred
   drag
      | x > 0 = first pred
      | x < 0 = first succ
      | otherwise = id


-- part 2

