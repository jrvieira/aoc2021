module Κ10 where

import Zero.Zero
import Data.List

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/10.txt"
   teqt "part 1" 26397 $ part1 input
   teqt "part 2" 288957 $ part2 input

main :: IO ()
main = do
   input <- parse <$> readFile "./input/10.txt"
   print $ part1 input
   print $ part2 input

parse = lines

-- part 1

part1 = sum . map (syntaxcheck . β)

data Err = Stack String | Illegal Char
   deriving Show

β :: String -> Err
β = go []
   where
   go stack [] = Stack stack
   go stack (x:xs)
      | x ∈ "({[<" = go (x : stack) xs
      | (s:ss) <- stack , x == co s = go ss xs
      | otherwise = Illegal x

syntaxcheck (Illegal ')') = 3
syntaxcheck (Illegal ']') = 57
syntaxcheck (Illegal '}') = 1197
syntaxcheck (Illegal '>') = 25137
syntaxcheck _ = 0

co '(' = ')'
co '[' = ']'
co '{' = '}'
co '<' = '>'
co x = error $ unwords ["impossible character", show x]

-- part 2

part2 = middle . filter (> 0) . map (score . β)

middle :: [Integer] -> Integer
middle x = sort x !! div (length x) 2

score :: Err -> Integer
score (Illegal _) = 0
score (Stack s) = foldl' (\n x -> 5 * n + autocomplete (co x)) 0 s

autocomplete ')' = 1
autocomplete ']' = 2
autocomplete '}' = 3
autocomplete '>' = 4
autocomplete _ = 0
