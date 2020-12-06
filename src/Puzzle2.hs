module Puzzle2 () where

inputFileName = "./data/puzzle_1.txt"

solve :: [Int] -> Int
solve xs = head [a * b * c | a <- xs, b <- xs, c <- xs, a + b + c == 2020]

stringToInt :: String -> Int
stringToInt = read

main = do
  content <- readFile inputFileName
  putStrLn $ show $ solve $ map stringToInt $ lines content
