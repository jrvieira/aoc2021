import System.Environment
import System.Process

import Data.List ( intercalate )

main :: IO ()
main = do
   (da:ya:_) <- (++ ["",""]) <$> getArgs
   ys <- head . words <$> readCreateProcess (shell "echo $(date -d \"\" '+%Y')") ""
   ds <- head . words <$> readCreateProcess (shell "echo $(date -d \"\" '+%d')") ""
   let y = validate [2015..read ys] $ if null ya then ys else ya
   let d = validate [1..25] . pad 2 '0' $ if null da then ds else da
   putStrLn $ "fetching y:" ++ y ++ " " ++ d
   out <- readCreateProcess (shell $ command y d) ""
   putStrLn out

sess :: String
sess = "53616c7465645f5f2afc3e3790d81e2e2554c4cc4ca4a5ae0d717104d9e27dfee643bd94e28b9c4bb58850172880f39b"

command :: String -> String -> String
command y d = intercalate " "
   [ "curl -# --cookie \"session=" ++ sess ++ "\""
   , "https://adventofcode.com/" ++ y ++ "/day/" ++ d ++ "/input"
   , "-o \"./input/" ++ d ++ ".txt\" --create-dirs"
   , "&& cat " ++ "./input/" ++ d ++ ".txt"
   ]

pad :: Int -> Char -> String -> String
pad n c s = replicate (max 0 $ n - length s) c ++ s

validate :: [Int] -> String -> String
validate r s
   | elem (read s) r = s
   | otherwise = error $ "invalid date: " ++ s
