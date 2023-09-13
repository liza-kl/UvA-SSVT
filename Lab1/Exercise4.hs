import Data.List 
{--
class Eq where
    (==) :: a -> a -> Bool
--

--}

ourContains:: Eq a => [a] -> a -> Bool
ourContains [] _ = False 
ourContains (x:xs) elemToCheck 
    | x == elemToCheck = True
    | otherwise = ourContains xs elemToCheck

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation list1 list2 = ourContains (permutations list1) list2 

isDerangementH :: Eq a => [a] -> [a] -> Bool 
isDerangementH [] [] = True 
isDerangementH (x:xs) (y:ys) 
    | x == y = False 
    | otherwise = isDerangementH xs ys

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement list1 list2 = isPermutation list1 list2 && isDerangementH list1 list2


