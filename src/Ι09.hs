module Ι09 where

import Zero.Zero
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/09.txt"
   teqt "part 1" 15 $ part1 input
   teqt "part 2" 1134 $ part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/09.txt"
   print $ part1 input
   print $ part2 input

parse :: String -> Grid
parse = grid . map (map digitToInt) . lines
   where
   grid :: [[Int]] -> Grid
   grid = gox (0,0) M.empty
      where
      gox _ m [] = m
      gox (x,y) m (l:ls) = gox (x,succ y) (goy (x,y) m l) ls
      goy _ m [] = m
      goy (x,y) m (n:ns) = goy (succ x,y) (M.insert (x,y) n m) ns

-- part 1

part1 g = sum $ map (succ . at g) (lows g)

type Pos = (Int,Int)
type Grid = Map Pos Int

lows :: Grid -> [Pos]
lows g = filter (low g) (M.keys g)

low :: Grid -> Pos -> Bool
low g p = all (> at g p) $ S.map (at g) (adjacents p)

at :: Grid -> Pos -> Int
at g p = M.findWithDefault 9 p g

adjacents :: Pos -> Set Pos
adjacents (x,y) = S.fromList $ zip [x,x,pred x,succ x] [succ y,pred y,y,y]

-- part 2

part2 = product . take 3 . sortBy (flip compare) . map size . basins
   where
   basins g = map (basin g S.empty) $ lows g

basin :: Grid -> Set Pos -> Pos -> Set Pos
basin g s p = S.foldl' (basin g') s' flow
   where
   flow :: Set Pos
   flow = S.filter ((< 9) . at g') $ adjacents p
   g' = M.withoutKeys g s'
   s' = S.insert p s


