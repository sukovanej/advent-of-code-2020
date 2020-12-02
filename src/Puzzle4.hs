module Puzzle4 () where

import           Data.List.Split

xor :: Bool -> Bool -> Bool
xor True True   = False
xor False False = False
xor True False  = True
xor False True  = True

inputFileName = "./data/puzzle_3.txt"

type Data = (Int, Int, Char, String)

parseData :: String -> Data
parseData xs = (read a, read b, parts !! 1 !! 0, parts !! 2)
  where parts = words xs
        (a:b:[]) = splitOn "-" $ parts !! 0

isValid :: Data -> Bool
isValid (p1, p2, ch, xs) = (xs !! (p1 - 1) == ch) `xor` (xs !! (p2 - 1) == ch)

main = do
  content <- readFile inputFileName
  putStrLn $ show $ length $ filter isValid $ map parseData $ lines content
