module Ο15 where

import Zero.Zero
import Data.Char
import Data.List
import Data.Map.Strict as M hiding ( take )
import Data.Bifunctor
import Algorithm.Search

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/15.txt"
   teqt "part 1" 40 $ part1 input
   teqt "part 2" 315 $ part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/15.txt"
   print $ part1 input
   print $ part2 input

type Pos = (Int,Int)
type Point = (Pos,Int)
type Grid = Map Pos Int

parse :: String -> Grid
parse = grid . fmap (fmap digitToInt) . lines
   where
   grid :: [[Int]] -> Grid
   grid = gox (0,0) mempty
      where
      gox _ s [] = s
      gox (x,y) s (l:ls) = gox (x,succ y) (goy (x,y) s l) ls
      goy _ s [] = s
      goy (x,y) s (n:ns) = goy (succ x,y) (M.insert (x,y) n s) ns

-- part 1

part1 g = maybe 0 fst $ aStar vertices cost heuristic goal start
   where
   vertices (x,y) = intersect (M.keys g) [(pred x,y),(x,pred y),(succ x,y),(x,succ y)]
   cost _ p = g ! p
   heuristic (x,y) = ((s-x) + (s-y))  -- # show (x,y)
   goal = (== (s,s))
   start = (0,0)
   s = pred $ length [ () | (0,_) <- M.keys g ]

-- part 2

part2 g = part1 (big 5)
   where
   big n = M.fromList $ concat [ cal x y <$> M.assocs g | x <- take n [0..] , y <- take n [0..] ]
   cal x y = bimap (bimap (+s*x) (+s*y)) ((!! (x+y)) . iterate (succ . flip mod 9))
   s = length [ () | (0,_) <- M.keys g ]
