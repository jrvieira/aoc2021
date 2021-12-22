module Λ11 where

import Zero.Zero
import Zero.Color
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/11.txt"
   teqt "part 1" 1656 $ part1 input
   teqt "part 2" 195 $ part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/11.txt"
   print $ part1 input
   print $ part2 input

parse :: String -> Frame
parse = frame . map (map digitToInt) . lines
   where
   frame :: [[Int]] -> Frame
   frame = gox (0,0) M.empty
      where
      gox _ m [] = m
      gox (x,y) m (l:ls) = gox (x,succ y) (goy (x,y) m l) ls
      goy _ m [] = m
      goy (x,y) m (n:ns) = goy (succ x,y) (M.insert (x,y) (0,toEnum n) m) ns

-- part 1

part1 = flashes . (!! 100) . iterate step

type Octopus = (Int,E)  -- (flashes,Energy)
type Pos = (Int,Int)
type Frame = Map Pos Octopus

data E = E | E1 | E2 | E3 | E4 | E5 | E6 | E7 | E8 | E9 | F
   deriving ( Eq, Enum )

instance Show E where
   show E = clr Yellow "0"
   show F = " "
   show e = clr Default $ clr Blue $ show $ fromEnum e

showframe :: Frame -> String
showframe f = '\n' : go (0,0)
   where
   go (x,y)
      | M.member (x,y) f = show (snd $ f M.! (x,y)) <> go (succ x,y)
      | M.member (0,succ y) f = "\n" <> go (0,succ y)
      | otherwise = "\n"

flashes :: Frame -> Int
flashes = sum . M.map fst

step :: Frame -> Frame
step = go . M.map su

su :: Octopus -> Octopus
su (f,F) = (f,F)
su (f,e) = (f,succ e)

go :: Frame -> Frame
go f
-- | False  # showframe f = undefined
   | null fls = f
   | otherwise = go $ charge $ flash f
   where
   flash  f = foldl' (flip $ M.adjust fl) f fls
   charge f = foldl' (flip $ M.adjust ch) f chs
   fls = [ pos | (pos,(_,F)) <- M.assocs f ]
   chs = concatMap adjacents fls

fl :: Octopus -> Octopus
fl (f,F) = (succ f,E)
fl (f,e) = (f,e)

ch :: Octopus -> Octopus
ch (f,F) = (f,F)
ch (f,E) = (f,E)
ch (f,e) = (f,succ e)

adjacents :: Pos -> [Pos]
adjacents (x,y) = [ (x',y') | x' <- [pred x..succ x] , y' <- [pred y..succ y] , (x,y) /= (x',y') ]

-- part 2

part2 = go 0
   where
   go :: Int -> Frame -> Int
   go n f
      | all (== E) $ M.map snd f = n
      | otherwise = go (succ n) (step f)
