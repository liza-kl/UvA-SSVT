{-- A permutation of a finite list is another finite list with the same elements,
but possibly in a different order. 
For example, [3,2,1] is a permutation of [1,2,3], but [2,2,0] is not. Write a function

isPermutation :: Eq a => [a] -> [a] -> Bool
that returns True if its arguments are permutations of each other.

Next, define some testable properties for this function, 
and use a number of well-chosen lists to test isPermutation .
You may assume that your input lists do not contain duplicates.
What does this mean for your testing procedure?
 --}
import Data.List 

ourContains:: Eq a => [a] -> a -> Bool
ourContains [] _ = False 
ourContains (x:xs) elemToCheck 
    | x == elemToCheck = True
    | otherwise = ourContains xs elemToCheck

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation list1 list2 = ourContains (permutations list1) list2 -- Can someone plase explain why the refatoring is working

{-- a permutation is a rearrangement of a collection of items --}
-- Repetitions are not allowed in permutations
 -- The nPr formula is, P(n, r) = n! / (n−r)!.

fact n = product [1..n] 
permutationProperty n r = fact n `div`fact (n - r)

-- TODO: Write QuickCheck Test Stuff 