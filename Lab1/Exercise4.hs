import Data.List
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
