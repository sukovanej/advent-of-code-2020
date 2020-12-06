module Day6 where

import           Data.Char
import           Data.List.Split
import qualified Data.Set        as S

inputFileName = "./data/day6.txt"

splitIntoGroups = splitOn "\n\n"

countUniqueInGroup' :: S.Set Char -> String -> Int
countUniqueInGroup' result [] = length result
countUniqueInGroup' result (x:xs) = if isAlpha x
                                       then countUniqueInGroup' (S.insert x result) xs
                                       else countUniqueInGroup' result xs
countUniqueInGroup = countUniqueInGroup' S.empty

groupToListOfAnswers :: String -> [String]
groupToListOfAnswers = (filter (/=[])) . (map $ filter isAlpha) . (splitOn "\n")

findIntersectingAswers :: [String] -> Int
findIntersectingAswers' result [] = length result
findIntersectingAswers' result (x:xs) = findIntersectingAswers' (S.intersection result (S.fromList x)) xs
findIntersectingAswers xs = findIntersectingAswers' (S.fromList $ head xs) xs

run = do
  content <- readFile inputFileName
  putStrLn . show . sum . (map countUniqueInGroup) . splitIntoGroups $ content
  putStrLn . show . sum . (map $ findIntersectingAswers . groupToListOfAnswers) . splitIntoGroups $ content
