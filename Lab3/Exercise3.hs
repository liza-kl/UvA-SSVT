
module Exercise3 where
import Data.List
import Test.QuickCheck
import FitSpec

{-- 
    Time Spent: 

    Approach:
    - Find all combinations of property subsets
    - Find the smallest subset property subset that is as strong as the initial set of properties
--}

propertyCombinations :: [String] -> [[String]]
propertyCombinations properties = tail (subsequences properties)

-- compareCombinationToAllProperties :: [String] -> [String] -> Bool
-- compareCombinationToAllProperties properties combination = do

-- findMinimalSubsets :: [String] -> [[String]] -> [[String]]
-- findMinimalSubsets properties combinations = do
--     filter (compareCombinationToAllProperties properties) combinations

main :: IO ()
main =  do
    let properties = ["prop_tenElements", "prop_firstElementIsInput", "prop_sumIsTriangleNumberTimesInput", "prop_linear", "prop_moduloIsZero"]
    let combinations = propertyCombinations properties
    print combinations
    -- let minimalSubsets = findMinimalSubsets properties combinations 
    -- print minimalSubsets