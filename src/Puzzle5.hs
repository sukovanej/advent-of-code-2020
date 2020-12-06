module Puzzle5 () where

inputFileName = "./data/puzzle_5.txt"
-- inputFileName = "./data/day_3_test.txt"

solve :: Int -> Int -> Int -> [String] -> [Char]
solve down right pos [] = []
solve down right pos xs  = el : (solve down right (pos + 1) (drop down xs))
  where l = length xs
        line = xs !! 0
        ll = length line
        el = line !! ((pos * right) `mod` ll)

numberOfTree :: [String] -> Int -> Int -> Int
numberOfTree xs down right = length . filter (== '#') $ solve down right 0 xs

main = do
  l <- readFile inputFileName >>= return . lines
  putStrLn $ show $ product $ map (uncurry (numberOfTree l)) [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
