module Exercise9 where
import Test.QuickCheck

{-- 

    A permutation of a finite list is another finite list with the same elements,
    but possibly in a different order. 
    For example, [3,2,1] is a permutation of [1,2,3], but [2,2,0] is not. Write a function

    isPermutation :: Eq a => [a] -> [a] -> Bool
    that returns True if its arguments are permutations of each other.

    Next, define some testable properties for this function, 
    and use a number of well-chosen lists to test isPermutation .
    You may assume that your input lists do not contain duplicates.
    What does this mean for your testing procedure?

    Time spent: 90 minutes

--}

import Data.List 

{-- A helper function to check if an element is in our list--}
ourContains:: Eq a => [a] -> a -> Bool
ourContains [] _ = False 
ourContains (x:xs) elemToCheck 
    | x == elemToCheck = True
    | otherwise = ourContains xs elemToCheck

{-- a permutation is a rearrangement of a collection of items --}
-- Repetitions are not allowed in permutations
-- The nPr formula is, P(n, r) = n! / (n−r)!.
isPermutation:: Eq a => [a] -> [a] -> Bool
isPermutation list1 list2 = ourContains (permutations list1) list2 && length list1 == length list2 

fact :: (Num a, Enum a) => a -> a
fact n = product [1..n] 

-- QuickCheck Properties 
-- To see if a complete inversion/derangement is accounted for.

permutationProperty :: Integral a => a -> a -> a
permutationProperty n r = fact n `div` fact (n - r)

isEmptyPermutation :: [Integer] -> Bool
isEmptyPermutation = isPermutation []

-- If you reverse a permutation list, it is still a permutation (but not necessary a derangement)
prop_invertability:: [Int] -> Bool
prop_invertability list = isPermutation list (reverse list)

-- General Associativity to see if swapped elements are correctly checked.
prop_associativity:: [Int] -> Bool
prop_associativity [] = isEmptyPermutation []
prop_associativity (x:xs) = isPermutation (x:xs) (xs ++ [x])

-- Identity to see if a list and it's identity are a valid permutation.
prop_identity:: [Int] -> Bool
prop_identity list = isPermutation list list

-- Adding another element to the checked list, which shouldn't be a permutation
-- of the original list.
prop_noPermutation:: [Int] -> Bool
-- An empty set can't be a permutation of itself
prop_noPermutation [] = not (isPermutation [] [1]) && not (isPermutation [1] [])
-- A set with one element in it can't be a permutation of itself
prop_noPermutation [a] = not (isPermutation [a] [a, 1])
prop_noPermutation (x:xs) = not (isPermutation (x:xs) ((x:xs) ++ [x]))

-- Generates random lists of unique Integers. We have to keep a reasonable size 
-- though, so test execution doesn't take too long.
uniqueIntList :: Gen [Int]
uniqueIntList = listOf arbitrary `suchThat` (\xs -> xs == nub xs && length xs < 10)


{-- Test Report
- All tests are passing, we can automate the test process (we are using QuickCheck for that). Basically
a permutation is given if all the necessary properties are satisfied.
- If the list does not contain duplicates, we can assume that all given lists 
can create permutations (as repetitions are not allowed in permutations)
-- We weren't sure about the well chosen integer lists but it's probably covered by the provided edge cases
in prop_noPermutation
--}

main :: IO ()
main = do
      quickCheck $ forAll uniqueIntList prop_invertability
      quickCheck $ forAll uniqueIntList prop_associativity
      quickCheck $ forAll uniqueIntList prop_identity
      quickCheck $ forAll uniqueIntList prop_noPermutation
