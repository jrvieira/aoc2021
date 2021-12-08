module Γ03 where

import Zero.Zero
import Data.List ( transpose, foldl' )
import Data.Bifunctor ( bimap )
import Control.Monad ( join )
import Control.Arrow ( (&&&) )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/03.txt"
   teqt "part 1" 198 $ part1 input
   teqt "part 2" 230 $ part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/03.txt"
   print $ part1 input
   print $ part2 input

parse :: String -> [Bin]
parse = map (map (== '1')) . lines

-- part 1

part1 = uncurry (*) . join bimap nat . (id &&& map not) . mode
   where
   mode = map ((> 0) . tend id 0) . transpose

type Bin = [Bool]

tend :: Enum e => (a -> Bool) -> e -> [a] -> e
tend _ z [] = z
tend p z (x:xs) = t $ tend p z xs
   where
   t = if p x then succ else pred

nat :: Bin -> Word
nat = foldl' (\n b -> 2 * n + (toEnum . fromEnum) b) 0

-- part 2

part2 bs = o2 t * co2 t
   where
   t = tree bs

-- binary tree where each node contains
-- the size of the set of numbers which
-- can be described by the current path

--               size        size 1    0
data Tree = Leaf Word | Node Word Tree Tree

-- build the tree

tree :: [Bin] -> Tree
tree bs = go n $ map nat bs
   where
   -- 2^n = most significant bit
   n = pred $ size $ head bs
   go :: Word -> [Word] -> Tree
   go n xs
      | n == 0    = Node s (Leaf $ size is) (Leaf $ size os)
      | otherwise = Node s (go (pred n) is) (go (pred n) os)
      where
      (s,is,os) = sblit n xs

-- n becomes the most significant index
-- exponent at base b in modulo b^(n-1)
-- this is unnecessary cleverness as we
-- could save on Bin -> Word conversion

sblit :: Word -> [Word] -> (Word,[Word],[Word])
sblit n = go 0 ([],[])
   where
   go s (is,os) [] = (s,is,os)
   go s (is,os) (x:xs)
      | mod x (2^(n+1)) >= 2^n = go (succ s) (x:is,os) xs
      | otherwise              = go (succ s) (is,x:os) xs

-- conditional traversal of binary ndes

o2 :: Tree -> Word
o2 t = go t 0
   where
   go (Leaf _) = id
   go (Node _ i o)
      | 0 <- root o      = go i . (1 +) . (2 *)
      | 0 <- root i      = go o . (2 *)
      | root i >= root o = go i . (1 +) . (2 *)
      | otherwise        = go o . (2 *)

co2 :: Tree -> Word
co2 = flip go 0
   where
   go (Leaf _) = id
   go (Node _ i o)
      | 0 <- root i      = go o . (2 *)
      | 0 <- root o      = go i . (1 +) . (2 *)
      | root o <= root i = go o . (2 *)
      | otherwise        = go i . (1 +) . (2 *)

root :: Tree -> Word
root (Leaf x)     = x
root (Node x _ _) = x

