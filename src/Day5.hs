module Day5 (run, rowCodeToNumber, columnCodeToNumber, seatCodeToSeatId) where

import           Data.List

inputFileName = "./data/day_5.txt"

charToBit :: Char -> Int
charToBit 'F' = 0
charToBit 'B' = 1
charToBit 'L' = 0
charToBit 'R' = 1

codeToNumber :: String -> Int
codeToNumber (x:[]) = charToBit x
codeToNumber (x:xs) = (charToBit x) * 2 ^ (length xs) + rowCodeToNumber xs

rowCodeToNumber = codeToNumber
columnCodeToNumber = codeToNumber

seatCodeToSeatId :: String -> Int
seatCodeToSeatId xs = (rowCodeToNumber rowCode) * 8 + (columnCodeToNumber columnCode)
  where (rowCode, columnCode) = splitAt 7 xs

getAllSeats :: String -> [Int]
getAllSeats = map seatCodeToSeatId . lines

getYourSeat :: [Int] -> Int
getYourSeat xs = findSeat sortedXs
  where sortedXs = sort xs
        findSeat (x:y:sxs) = if x + 2 == y then x + 1 else findSeat (y:sxs)

run = do
  content <- readFile inputFileName
  allSeats <- return $ getAllSeats content
  putStrLn . show . maximum $ allSeats
  putStrLn . show $ getYourSeat allSeats
