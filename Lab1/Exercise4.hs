module Exercise4 where

-- Time Spent: ~90 Mins

import Data.List
import Test.QuickCheck
import Data.Maybe

ourContains:: Eq a => [a] -> a -> Bool
ourContains [] _ = False
ourContains (x:xs) elemToCheck
    | x == elemToCheck = True
    | otherwise = ourContains xs elemToCheck

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation list1 = ourContains (permutations list1)

isDerangement' :: Eq a => [a] -> [a] -> Bool
isDerangement' [] [] = True
isDerangement' (x:xs) (y:ys)
    | x == y = False
    | otherwise = isDerangement' xs ys

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement list1 list2 = isPermutation list1 list2 && isDerangement' list1 list2


{-- 
    Give a Haskell implementation of a function  deran:: Int -> [[Int]]  that generates a list 
    of all derangements of the list  [0..n-1]

    Example of a derangement: [1,2,3] and [3,1,2] would be a derangement of the original list.
--}

deran :: Int -> [[Int]]
deran num =  filter (isDerangement [0..num - 1]) $ permutations [0 .. num - 1]

{-- 
    Properties 2 or ideally 3,
    Well chosen list is about "edge cases"

    In combinatorial mathematics, a derangement is a permutation of the elements of a set in which no element
    appears in its original position. In other words, a derangement is a permutation that has no fixed points.
--}

factorial num
    | num < 0 = error "Negative numbers are not possible"
    | num == 1 = 1
    | num == 0 = 1
    | otherwise = num * factorial (num - 1)

derangement :: Int -> Int
derangement 0 = 1
derangement 1 = 0
derangement n = (n - 1) * (derangement (n - 1) + derangement (n - 2))

-- QuickCheck properties

isListAllDerangement :: [[Int]] -> [Int] -> Bool
isListAllDerangement xs base = foldr ((&&) . isDerangement base) True xs

-- The number of permutations for an empty set equals to the factorial of 0 which is 1. 
-- Since no element (of the empty set) can be found that retains its original position this means that this permutation is also a derangement. 
-- That is why the number of derangements for an empty set is one.
prop_numOf0PermutationsIsOne :: Property
prop_numOf0PermutationsIsOne = property ( length ( deran 0 ) == 1 )

-- Since the list contains a single integer there isn't really a way to move that single integer around or even making any derangements out of that. 
-- Example: A "permutation" of [1] would always result in [1] which isn't a derangement thus having 0 derangements.
prop_numOf1PermutationsIsZero :: Property
prop_numOf1PermutationsIsZero = property( length ( deran 1 ) == 0 )

-- The list of all permutations is also always including the entire list of all derangements. 
-- Some permutations are no derangements though so the amount of derangements must be always smaller or equal the amount of permutations though.
prop_numOfDerangmentsLessEqThanPerms :: Int -> Property
prop_numOfDerangmentsLessEqThanPerms num = property ( length ( deran num ) <= length ( permutations [0 .. num - 1]) )

-- Checks if the reverse of a list with an even number of elements is a derangement of a given list.
prop_reverseEvenNumberSizedList :: Eq a => [a] -> Property
prop_reverseEvenNumberSizedList list = length list > 1 && length (nub list) == length list ==> even (length list) == isDerangement list (reverse list)

-- Checks if the reverse list of a derangement from a given list is not a derangement.
prop_evenNonReversibleDerangement :: Eq a => [a] -> Property
prop_evenNonReversibleDerangement list = length list > 1 && length (nub list) == length list ==> not (isDerangement getList (reverse getList)) where getList = take 1 (deran (length list))

-- Checking if the number of calculated derangements lines up with the length of the list for the generated derangements.
prop_numOfDerangements :: Int -> Property
prop_numOfDerangements num = property ( length (deran num) == derangement num )

-- Checks if the "isDerangement" method is working. "Deran" calculates all derangements of the list [0..(n-1)].
-- We use a helper function to check if every generated derangement is actually a derangement with the checker function.
-- We only consider deran 1 or higher since negative numbers shouldn't be possible and 0 can't be possible since the list would then go from 0 to -1. 
prop_isBasicDerangement :: Int -> Property
prop_isBasicDerangement num = num > 0 ==> isListAllDerangement getList [0..(num - 1)] where getList = deran num

-- Values and value sets could be non-restrictive in theory.
-- But due to factorial time complexity test execution would need a veeeeeeeeery long amount of time.
-- I think you got better stuff to do than to wait for that. :D
main :: IO()
main = do
   quickCheck prop_numOf0PermutationsIsOne
   quickCheck prop_numOf1PermutationsIsZero
   quickCheck $ prop_numOfDerangmentsLessEqThanPerms 3
   quickCheck $ prop_numOfDerangements 3
   quickCheck $ prop_reverseEvenNumberSizedList [1..5]
   quickCheck $ prop_evenNonReversibleDerangement [1..5]
   verboseCheck $ prop_isBasicDerangement 5

{- 

    Question 4.4 

    Property 1 and 2 have equal strength and are stronger than the rest of the properties.
    This is due to them only checking for a singular value which corresponds an edge case.

    Property 3, 4 and 5 also have equal strength but are weaker than 1/2 because they apply for the entire set of integer values.
    That makes them automatically weaker than property 1/2.


    Question 4.5 

    This test can be automatically tested (via QuickCheck) but only a small subset of the possible parameters can be checked. 
    This is due to factorial (time) complexity and infinitely large number of positive integer numbers.
    In theory we could let it run without any restrictions to certain values or value sets but then we would quickly run into problems
    regarding calculation complexity. That is why we restrict the values here to have viable test execution times.

-}