module Puzzle3 () where

import           Data.List.Split

inputFileName = "./data/puzzle_3.txt"

type Data = (Int, Int, Char, String)

parseData :: String -> Data
parseData xs = (read a, read b, parts !! 1 !! 0, parts !! 2)
  where parts = words xs
        (a:b:[]) = splitOn "-" $ parts !! 0

isValid :: Data -> Bool
isValid (min, max, ch, xs) = occurences >= min && occurences <= max
  where occurences = length $ filter (==ch) xs

main = do
  content <- readFile inputFileName
  putStrLn $ show $ length $ filter isValid $ map parseData $ lines content
