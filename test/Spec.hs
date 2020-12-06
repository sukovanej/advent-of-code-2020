import Test.Tasty

import Day4Spec
import Day5Spec

main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" $ 
    []
    -- ++ [testInvalidPassports, testValidPassports]
    ++ [testRowCodeToNumber, testColumnCodeToNumber, testSeatCodeToSeatId]
