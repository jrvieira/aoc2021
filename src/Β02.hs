module Β02 where

import Zero.Zero
import Data.Foldable ( foldl' )
import Data.Bifunctor ( first, second )
import Control.Arrow ( (&&&) )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/02.txt"
   teqt "part 1" 150 $ part1 input
   teqt "part 2" 900 $ part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/02.txt"
   print $ part1 input
   print $ part2 input

parse :: String -> [(String,Int)]
parse = map ((head &&& read . last) . words) . lines

-- part 1

part1 = uncurry (*) . foldl' move (0,0)

type Pos = (Int,Int)
type Mov = (String,Int)

move :: Pos -> Mov -> Pos
move pos (k,n)
   | "forward" <- k = first (+ n) pos
   | "up"      <- k = second (subtract n) pos
   | "down"    <- k = second (+ n) pos
   | _         <- k = error $ unwords ["invalid command",show k,show n]

-- part 2

part2 = uncurry (*) . π . foldl' mov' (Σ (0,0) 0)

data Stt = Σ { π :: Pos , α :: Int }

mov' :: Stt -> Mov -> Stt
mov' s@(Σ (x,y) aim) (k,n)
   | "forward" <- k = s { π = (x+n,y+n*aim) }
   | "up"      <- k = s { α = aim - n }
   | "down"    <- k = s { α = aim + n }
   | _         <- k = error $ unwords ["invalid command",show k,show n,show aim]


