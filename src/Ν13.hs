module Ν13 where

import Zero.Zero
import Data.List
import Data.Bifunctor
import Data.Ord
import Data.Set ( Set )
import Data.Set qualified as S
import Control.Monad

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/13.txt"
   teqt "part 1" 17 $ part1 input
   part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/13.txt"
   print $ part1 input
   part2 input

parse :: String -> (Set Point,[Fold])
parse s = (ps,fs)
   where
   ps = S.fromList $ fmap (join bimap read . second tail . break (== ',')) dots
   fs = f . drop 11 <$> folds
   (dots,folds) = fmap tail . break (== "") . lines $ s
   f ('x':'=':n) = X $ read n
   f ('y':'=':n) = Y $ read n
   f i = error $ unwords ["invalid fold instruction",i]

type Point = (Int,Int)
data Fold = X Int | Y Int

-- part 1

part1 (ps,fs) = S.size $ foldl' fold ps (take 1 fs)

fold :: Set Point -> Fold -> Set Point
fold ps f
   | X n <- f = S.filter ((< n) . fst) ps ∪ S.map (first  $ reflect n) (S.filter ((> n) . fst) ps)
   | Y n <- f = S.filter ((< n) . snd) ps ∪ S.map (second $ reflect n) (S.filter ((> n) . snd) ps)
   where
   reflect n c = n-(c-n)

-- part 2

part2 (ps,fs) = show' $ foldl' fold ps fs

show' :: Set Point -> IO ()
show' ps = putStrLn $ go (0,0)
   where
   go (x,y)
      | y > my = "\n"
      | x > mx = '\n' : go (0,succ y)
      | S.member (x,y) ps = '#' : go (succ x,y)
      | otherwise  = '.' : go (succ x,y)
   mx = fst $ maximumBy (comparing fst) ps
   my = snd $ maximumBy (comparing snd) ps
