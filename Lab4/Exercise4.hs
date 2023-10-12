module Exercise4 where
import Test.QuickCheck
import Data.List
import System.Random
import SetOrd
import Denis.Exercise1


-- TODO Documentation, Test for 2nd part, Short Test Report.
-- Time estimate 90 Mins. (Mainly due to debugging)


{-

    A serial relation is present, when for a relation (x,y), 
    both x and y have a corresponding element in the given domain. ([a])
    So we just recursively check for every relation inside of Rel a,
    we have a corresponding element in the domain, using the given "inSet" function.
    ("inSet" acts as a "contains" function in this case and other implementations could've been considered,
    but why make life unnecessarily harder?)

-}
type Rel a = [(a,a)]

isSerial:: (Eq a, Ord a) => [a] -> Rel a -> Bool
isSerial domain [] = True
isSerial domain (x:xs) = (inSet (fst x) domainSet && inSet (snd x) domainSet) && isSerial domain xs
                where domainSet = list2set domain


-- QuickCheck Properties

-- Serial relations have to be an endorelator (binary relation to itself) to the original domain set.
-- To check this we test if the 2-length pairs generated by the subsequences (cartesian product) of the domain are serial to the domain, which they should be.
-- This function creates all of the endorelator pairs of a given domain value list to use as the relation for this property check.
getEndorelatorPairs:: [a] -> Rel a
getEndorelatorPairs domainValues = getSubsequencePairs (filter (\xs -> length xs == 2) (subsequences domainValues))

-- Converting the double-list format of the endorelator pairs into the correct relation data type.
-- This function assumes that each element in the sublist is only two elements long, since a relation is 2 elements long.
-- Also before using this function the subsequences are already in this format. Example: [[1,2], [2,3], [3,4]]
getSubsequencePairs:: [[a]] -> Rel a
getSubsequencePairs = map (\ x -> (head x, last x))

-- Now this properties uses an arbitrarily generated domain (Integer list) and its endorelator
-- to check if the property of the endorelator being serial towards its domain is true or not.
prop_isEndorelatorSerial:: Ord a => [a] -> Bool
prop_isEndorelatorSerial domain = isSerial domain (getEndorelatorPairs domain)

genIntLists:: Gen [Int]
genIntLists = arbitrary

genListOfInts :: Gen [Int]
genListOfInts = do
    n <- choose (0, 5)
    vectorOf n arbitrary

setToList:: Set a -> [a]
setToList (Set []) = []
setToList (Set (x:xs)) = x : setToList (Set xs)

setLength:: Set a -> Int
setLength set = length (setToList set)

uniqueIntList :: Gen [Int]
uniqueIntList = do
    set <- suchThat generateSets' (\s -> setLength s >= 3 && setLength s < 8)
    let setList = setToList set
    return setList

hasNonSerialElement:: (Eq a, Ord a) => [a] -> Rel a -> Bool
hasNonSerialElement domain [] = True
hasNonSerialElement domain (x:xs) = (inSet (fst x) domainSet && inSet (snd x) domainSet) && isSerial domain xs
                where domainSet = list2set domain


{-
    An "inverse serial relation" is the opposite concept, where every element has a non-empty "predecessor neighborhood," meaning that for each element x, there is at least one element y such that y is related to x.
-}
prop_hasNonSerialElement:: (Eq a, Ord a) => [a] -> Bool
prop_hasNonSerialElement domain = hasNonSerialElementinDomain domain (getEndorelatorPairs domain)

hasNonSerialElementinDomain:: (Eq a, Ord a) => [a] -> Rel a -> Bool
hasNonSerialElementinDomain domain [] = True
hasNonSerialElementinDomain domain ((x,y):xs) = (length [e | e <- domain, e /= x, e /= y] > 0) && hasNonSerialElementinDomain domain xs

{-
    For the relation R = {(x,y) | x = y % n} it can be proven by induction that it is indeed serial.

    For the base case n=1
    If we take the modulo of any integer number by one, it always equals 0.
    So therefore we can choose any y for the given x = 0, therefore fulfilling the base case.

    If we consider the hypothesis to be true, we now try to check if the statement holds true for n+1 as well.
    
    To prove this, we consider two cases.

    If y < (n+1), then y can be just x itself. 
    If you take the modulo of a number by a larger quotient it always equals the dividend.
    Thus if we set y=x -> x % (n+1), (if n+1 > y), is just x and we have a relating element.
    Thus the hypothesis holds true in this case.

    If you consider y >= (n+1),
    then you can "subtract" (n+1) any number of times until we arrive at the case that y < (n+1).
    This is since in this case, y % (n+1) = y - i*(n+1) % (n+1).
    Example. If y = 6 and n = 4, then 6 % 4 = (6 - 4) % 4 = 2.
    Therefore this case then transforms into the first case, which we already proven.
    Thus this case also holds true.

    Therefore both cases hold true and the induction step is proven. 
    
    With this R = {(x,y) | x = y % n} is proven to be serial. q.e.d. □

    (I really hate induction... Please don't do this again...)

-}

main :: IO()
main =
    do
        quickCheck $ forAll genListOfInts prop_isEndorelatorSerial
        quickCheck $ forAll uniqueIntList prop_hasNonSerialElement