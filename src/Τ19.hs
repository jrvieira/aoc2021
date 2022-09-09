module Τ19 where

import Zero.Zero

import Data.List ( partition )
import Data.List.Split ( splitOn )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/19.txt"
   teqt "part 1" 79 $ part1 input
-- teqt "part 2" mempty $ part2 input

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

-- beacons are lists of distances
part1 :: [[Point]] -> Int
part1 = length . go [] . concatMap beacons
   where
   go acc [] = acc
   go acc (b:bs)
      | False  # show (length as) = undefined  -- should always be 0
      | Just a <- match ???
      where
      (as,as') = partition (isJust $ match b) acc

newtype Beacon = Beacon { δ :: [[Val]] }

instance Semigroup Beacon where
   Beacon a <> Beacon b = Beacon (a <> b)

match :: Beacon -> Beacon -> Maybe Beacon
match (Beacon a) (Beacon b)
   | any (uncurry $ (>= 12) . length , (∩)) $ (,) <$> a <*> b = Just $ Beacon (a <> b)
   | otherwise = Nothing

-- a beacon is defined by the distances between itself and surrounding beacons
-- implementation detail: each beacon keeps a 0.0 distance to itself
beacons :: [Point] -> [Beacon]
beacons ps = map (Beacon . (: [])) $ go [] ps
   where
   go acc [] = acc
   go acc (x:xs) = go ((distance x <$> ps) : acc) xs

distance :: Point -> Point -> Val
distance a b = sqrt ( (x b - x a)^2 + (y b - y a)^2 + (z b - z a)^2 )


-- part 2

