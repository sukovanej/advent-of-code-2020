module Puzzle1 () where

inputFileName = "./data/puzzle_1.txt"

solve :: [Int] -> Int
solve xs = head [a * b | a <- xs, b <- xs, a + b == 2020]

stringToInt :: String -> Int
stringToInt = read

main = do
  content <- readFile inputFileName
  putStrLn $ show $ solve $ map stringToInt $ lines content
