module Μ12 where

import Zero.Zero
import Data.List
import Data.Char
import Data.Map.Strict ( Map )
import Data.Map.Strict qualified as M
import Control.Arrow

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/12.txt"
   teqt "part 1" 226 $ part1 input
   teqt "part 2" 3509 $ part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/12.txt"
   print $ part1 input
   print $ part2 input

parse :: String -> Map Cave [Cave]
parse = rmap . map (fmap tail . break (== '-')) . lines

-- intermediate step: reciprocal vertex index

rmap :: [(Cave,Cave)] -> Map Cave [Cave]
rmap = foldl' (flip go) mempty
   where
   go (cx,cy) = M.insertWith (<>) cy [cx] . M.insertWith (<>) cx [cy]

-- part 1

part1 = paths . tree

type Cave = String
data Tree = Start [Tree] | Node [Tree] | End
   deriving ( Show )

-- build cave paths tree

tree :: Map Cave [Cave] -> Tree
tree m = Start $ go m <$> m M.! "start"
   where
   go _ "start" = Node []  -- can't go back to the start
   go _ "end" = End
   go m cave@ ~(c:_)
      | not $ M.member cave m = Node []  -- dead end
      | isUpper c = Node $ go m <$> m M.! cave
      | otherwise = Node $ go (M.delete cave m) <$> m M.! cave

-- count leaves

paths :: Tree -> Int
paths (Start cs) = sum (paths <$> cs)
paths (Node cs) = sum (paths <$> cs)
paths End = 1

-- part 2

part2 = uncurry (+) . (sum . map paths . trees &&& paths . tree)

data Visited = None Cave | Once Cave | Twice

trees :: Map Cave [Cave] -> [Tree]
trees m = [ Start $ go (m,None k) <$> m M.! "start" | k <- filter (isLower . head) $ M.keys m ]  -- one tree for each small cave
   where
   go _ "start" = Node []  -- can't go back to the start
   go (_,Twice) "end" = End
   go _ "end" = Node []
   go (m,v) cave@ ~(c:_)
      | isUpper c = Node $ go (m,v) <$> m M.! cave
      | None k <- v , cave == k = Node $ go (m,Once k) <$> m M.! cave -- first visit
      | Once k <- v , cave == k = Node $ go (M.delete cave m,Twice) <$> m M.! cave -- second visit
      | not $ M.member cave m = Node []  -- dead end
      | otherwise = Node $ go (M.delete cave m,v) <$> m M.! cave
