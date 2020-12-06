module Day4 ( parseFields
            , isEntryValid2
            , run
            ) where

import           Data.Char
import           Data.List.Split

type Field = (String, String)

inputFileName = "./data/day_4.txt"

contentToEntries = splitOn "\n\n"

tuplify2 [x, y] = (x,y)

parseFields :: String -> [Field]
parseFields xs = map (tuplify2 . splitOn ":") $ words xs

isEntryValid :: [Field] -> Bool
isEntryValid fields = all (\x -> x `elem` fieldNames) mandatoryFields
  where mandatoryFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
        fieldNames = map fst fields

isEntryValid2 :: [Field] -> Bool
isEntryValid2 xs = isEntryValid xs && all (uncurry isFieldValid) xs

isFieldValid :: String -> String -> Bool
isFieldValid "byr" xs = length xs == 4 && n >= 1920 && n <= 2002 where n = read xs
  -- (Birth Year) - four digits; at least 1920 and at most 2002.
isFieldValid "iyr" xs = length xs == 4 && n >= 2010 && n <= 2020 where n = read xs
  -- (Issue Year) - four digits; at least 2010 and at most 2020.
isFieldValid "eyr" xs = length xs == 4 && n >= 2020 && n <= 2030 where n = read xs
  -- (Expiration Year) - four digits; at least 2020 and at most 2030.
isFieldValid "hgt" xs = all isDigit n' && u `elem` ["cm", "in"] &&
                        (u == "cm" && n >= 150 && n <= 193 || u == "in" && n >= 59 && n <= 76)
  where (n', u) = splitAt (length xs - 2) xs
        n = read n'
 -- (Height) - a number followed by either cm or in:
 -- If cm, the number must be at least 150 and at most 193.
 -- If in, the number must be at least 59 and at most 76.
isFieldValid "hcl" xs = d == "#" && length l == 6 && all (\x -> isDigit x || x `elem` ['a'..'f']) l
  where (d, l) = splitAt 1 xs
 -- (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
isFieldValid "ecl" xs = xs `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
 -- (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
isFieldValid "pid" xs = length xs == 9 && all isDigit xs
 -- a nine-digit number, including leading zeroes.
isFieldValid "cid" _  = True

run = do
  content <- readFile inputFileName
  putStrLn $ show $ length $ filter isEntryValid2 $ map parseFields $ contentToEntries $ content
