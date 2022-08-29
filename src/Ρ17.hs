module Ρ17 where

import Zero.Zero

import Data.List ( lookup, findIndex, groupBy )
import Data.Bifunctor ( bimap, first )
import Data.Maybe ( mapMaybe )
import Data.Function ( on )
import Control.Arrow ( (&&&) )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/17.txt"
   teqt "part 1"  45 $ part1 input
   teqt "part 2" 112 $ part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/17.txt"
   print $ part1 input
   print $ part2 input

parse :: String -> [Int]
parse = map read . alternate . tail . groupBy (on (==) (∉ ('-' : ['0'..'9'])))
   where
   alternate [] = []
   alternate (a:as) = a : alternate (drop 1 as)

part1 :: [Int] -> Int
part1 ~[_,_,yMin,yMax] = triangular $ max yMax (pred $ abs yMin)

triangular :: Int -> Int
triangular n = div (succ n * n) 2

-- part 2

data Position = P Int Int
   deriving (Eq, Ord)
data Velocity = V Int Int
   deriving (Eq, Ord)
type State = (Position,Velocity)

instance Show Velocity where
   show (V x y) = '\n' : unwords (show <$> [x,y])

part2 :: [Int] -> Int
part2 ~[xMin,xMax,yMin,yMax] = length $ unique hits

   where

   hits :: [[State]]
   hits = filter (any hit) shots

   hit :: State -> Bool
   hit (P x y,_) = x >= xMin && x <= xMax && y >= yMin && y <= yMax

   shots :: [[State]]
   shots = map (takeWhile inside . iterate step) poss

   inside :: State -> Bool
   inside (P x y,_) = x <= xMax && y >= yMin

   poss :: [State]
   poss = [(P 0 0,V x y) |
      x <- [minimum $ mapMaybe tn [xMin..xMax]..xMax] ,
      y <- [yMin..max yMax (pred $ abs yMin)] ]

tn :: Int -> Maybe Int
tn x = findIndex (abs x ==) $ takeWhile (<= x) (map triangular [0..])

step :: State -> State
step (P px py,V vx vy) = (P (px+vx) (py+vy),V (max 0 (pred vx)) (pred vy))

