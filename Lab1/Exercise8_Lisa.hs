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


--- Exercise 8

containsDivisor :: Int -> [Int] -> Bool
containsDivisor _ [] = False 
containsDivisor num (x:xs) 
    | num `mod` x == 0 = True
    | otherwise = containsDivisor num xs  
    
listOfPrimes = [x | x <- [2..100], isPrime x ]

multiplyAdd1 :: [Int] -> Int
multiplyAdd1 list1 = (foldr (*) 1 list1 ) + 1

isPrime :: Int -> Bool 
isPrime 1 = False
isPrime num = not (containsDivisor num [2..(num-1)])

counterexamples :: [([Int], Int)]
counterexamples = filter (\(subs, p) -> not(isPrime p)) ( map (\subs -> (subs, multiplyAdd1 subs) ) (subsequences listOfPrimes) )