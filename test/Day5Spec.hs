module Day5Spec ( testRowCodeToNumber
                , testColumnCodeToNumber
                , testSeatCodeToSeatId
                ) where

import           Day5

import           Test.Tasty
import           Test.Tasty.HUnit

testRowCodeToNumberParams = [ ("FBFBBFF", 44) ]

testColumnCodeToNumberParams = [ ("RLR", 5) ]

testRowCodeToNumber = testGroup "Check invalid passports" $
    map (\i -> testCase ("test case for " ++ (fst i)) $ (rowCodeToNumber (fst i)) @?= (snd i)) testRowCodeToNumberParams

testColumnCodeToNumber = testGroup "Check invalid passports" $
    map (\i -> testCase ("test case for " ++ (fst i)) $ (columnCodeToNumber (fst i)) @?= (snd i)) testColumnCodeToNumberParams

testSeatCodeToSeatId = testCase "seatCode" $ (seatCodeToSeatId "FBFBBFFRLR") @?= 357
