module Τ19 where

import Zero.Zero

import Data.List ( tails )
import Data.List.Split ( splitOn )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/19.txt"
   teqt "part 1" mempty $ part1 input
-- teqt "part 2" "" $ part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/19.txt"
   print $ part1 input
-- print $ part2 input

parse :: String -> [[Point]]
parse = map (map point . tail . lines) . splitOn "\n\n"

type Val = Double
data Point = Point { x :: Val , y :: Val , z :: Val }
   deriving ( Show, Eq, Ord )

point :: String -> Point
point s = Point x y z where (x,y,z) = read ('(' : s <> ")") :: (Val,Val,Val)

-- part 1

-- here scanners are identified by the distances between all pairs of beacons in range
part1 :: [[Point]] -> [[Val]]
part1 = map distances

adjacent :: [Val] -> Bool
adjacent = undefined

distances :: [Point] -> [Val]
distances = map (uncurry distance) . pairs

distance :: Point -> Point -> Val
distance a b = sqrt ( (x b - x a)^2 + (x b - x a)^2 + (z b - z a)^2 )

pairs :: [a] -> [(a,a)]
pairs l = [ (a,b) | (a:bs) <- tails l , b <- bs ]

-- part 2

