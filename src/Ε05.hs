module Ε05 where

import Zero.Zero
import Data.Foldable ( foldl' )
import Data.Map.Strict as Map ( Map, empty, insertWith, filter )
import Control.Arrow ( (&&&) )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/05.txt"
   teqt "part 1"  5 $ part1 input
   teqt "part 2" 12 $ part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/05.txt"
   print $ part1 input
   print $ part2 input

parse :: String -> [Line]
parse = map (((head &&& last) . take 2 &&& (head &&& last) . drop 2) . parseNums) . lines

type Point = (Int,Int)
type Line = (Point,Point)

-- part 1

part1 = length . intersections . Prelude.filter orthogonal

intersections :: [Line] -> Map Point Int
intersections = Map.filter (> 1) . foldl' count empty . concatMap interpolate

count :: Map Point Int -> Point -> Map Point Int
count m p = insertWith (+) p 1 m

orthogonal :: Line -> Bool
orthogonal ((x1,y1),(x2,y2)) = x1 == x2 || y1 == y2

interpolate :: Line -> [Point]
interpolate ((x1,y1),(x2,y2)) = take steps $ zip xs ys
   where
   xs = let f = if x1 <= x2 then succ else pred in cycle [x1,f x1..x2]
   ys = let f = if y1 <= y2 then succ else pred in cycle [y1,f y1..y2]
   steps = succ $ max (abs $ x1-x2) (abs $ y1-y2)

-- part 2

part2 = length . intersections

