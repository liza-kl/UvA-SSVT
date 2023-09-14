import Data.List
import Data.Maybe
{--
class Eq where
    (==) :: a -> a -> Bool
--
example of a derangement: permutation and not 1,2,3 

--}

ourContains:: Eq a => [a] -> a -> Bool
ourContains [] _ = False
ourContains (x:xs) elemToCheck
    | x == elemToCheck = True
    | otherwise = ourContains xs elemToCheck

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation list1 = ourContains (permutations list1)

isDerangementH :: Eq a => [a] -> [a] -> Bool
isDerangementH [] [] = True
isDerangementH (x:xs) (y:ys)
    | x == y = False
    | otherwise = isDerangementH xs ys

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement list1 list2 = isPermutation list1 list2 && isDerangementH list1 list2


{-- 
Give a Haskell implementation of a function  deran:: Int -> [[Int]]  that generates a list 
of all derangements of the list  [0..n-1]
--}

deran :: Int -> [[Int]]
deran num =  filter (isDerangement [0..num - 1]) $ permutations [0 .. num - 1]

{-- Properties 2 or ideally 3,
well chosen ist is about "edge cases"

In combinatorial mathematics, a derangement is a permutation of the elements of a set in which no element
appears in its original position. In other words, a derangement is a permutation that has no fixed points.
--}

factorial num
    | num < 0 = error "Negative numbers are not possible"
    | num == 1 = 1
    | num == 0 = 1
    | otherwise = num * factorial (num - 1)


prop_numOf0PermutationsIsOne :: Bool
prop_numOf0PermutationsIsOne = length ( deran 0 ) == 1

prop_numOf1PermutationsIsOne :: Bool
prop_numOf1PermutationsIsOne = length ( deran 1 ) == 1;

prop_numOfDerangmentsLessEqThanPerms :: Int -> Bool
prop_numOfDerangmentsLessEqThanPerms num = length ( deran num ) <= length ( permutations [0 .. num - 1]) where types = num:: Int

