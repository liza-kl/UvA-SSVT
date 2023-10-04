
module Exercise3 where
import Data.List
import Test.QuickCheck
import FitSpec

{-- 
    Time Spent: 

    Approach:
    - Find all combinations of property subsets
    - Find the smallest property subset that is as strong as the initial set of properties
    - meaning that there are no survivors
--}

propertyCombinations :: [a -> Integer -> Bool] -> [[a -> Integer -> Bool]]
propertyCombinations properties = tail (subsequences properties)

compareCombinationToAllProperties :: [a -> Integer -> Bool] -> [a -> Integer -> Bool] -> Bool
compareCombinationToAllProperties properties combination = True

findMinimalSubsets :: [a -> Integer -> Bool] -> [[a -> Integer -> Bool]] -> [[a -> Integer -> Bool]]
findMinimalSubsets properties combinations = do
    filter (compareCombinationToAllProperties properties) combinations

main :: IO ()
main =  do
    let properties = [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput, prop_linear, prop_moduloIsZero]
    let combinations = propertyCombinations properties
    let minimalSubsets = findMinimalSubsets properties combinations 
    print minimalSubsets