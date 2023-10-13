module Exercise4 where
import Test.QuickCheck
import Data.List
import System.Random
import SetOrd
import Exercise1

-- Time estimate 140 Mins. (Mainly due to debugging and induction proof thinking)

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

-- Generates fixed sized integer lists for better computability.
genListOfInts :: Gen [Int]
genListOfInts = do
    n <- choose (0, 5)
    vectorOf n arbitrary

-- Helper functions for set operations.
setToList:: Set a -> [a]
setToList (Set []) = []
setToList (Set (x:xs)) = x : setToList (Set xs)

setLength:: Set a -> Int
setLength set = length (setToList set)

-- Creates a at least 3 long integer list (until a given limit, this case 8 long, just to be able to process it)
-- This integer list also has no duplicate elements.
-- We need a unique, at least 3 integer long list, for the second property, since that only holds for these types of lists.
uniqueIntList :: Gen [Int]
uniqueIntList = do
    set <- suchThat generateSets' (\s -> setLength s >= 3 && setLength s < 8)
    let setList = setToList set
    return setList

{-

    This function checks if for a domain of more than 2 elements a relation always has at least one element which isn't part of the relation but part of the domain.
    For example, if the domain is [1,4,5] and the relation is [1,5], the relation is serial and has the [4] element which isn't in the relation.

-}
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
    If we take the modulo of any integer number by one, it always equals 0, since every number is divisible by one.
    So therefore we can choose any value y for the given x = 0. 
    Thus the base case is fulfilled.

    If we consider the hypothesis to be true, we now try to check if the statement holds true for n+1 as well.
    
    To prove this, we consider three cases.

    If y < (n+1) and y >= 0, then always x = y. 
    If you take the modulo of a number by a larger quotient it always equals the dividend.
    Therefore in this case y % (n+1) = y => y = x, because y is not fully divisible by (n+1) 
    and the full divisor rest has to be taken into account.
    Since the hypothesis also presumes that y ∈ A, therefore x ∈ A as well, because y = x.
    Thus the hypothesis holds true in this case.

    If you consider y >= (n+1),
    then you can "subtract" (n+1) any number of times until we arrive at the case that y < (n+1).
    This is the case since, y % (n+1) = y - i*(n+1) % (n+1). (i is integer >= 0)
    Example. If y = 6 and n = 4, then 6 % 4 = (6 - 4) % 4 = 2 % 4 = 2.
    Therefore this case then transforms into the first case, which we already proven.
    Thus this case also holds true.

    For cases where y < (n+1) and y < 0,
    then the same argument applies as with the second case, but we could then "add" (n+1) 
    any number of times until we arrive at the case that y < (n+1).
    This is the case since, y % (n+1) = y + i*(n+1) % (n+1). (i is integer >= 0)
    For example -11 % 7 = -4 % 7 = 3 % 7 = 3.
    Therefore this case then transforms into the first case, which we already proven.
    Thus this case also holds true.

    Therefore all cases hold true and the induction step is proven. 
    
    With this R = {(x,y) | x = y % n} is proven to be serial (every y is connected to an x). q.e.d. □

    (I really hate induction... Please don't do this again...)

-}

{-

    To test if this relation is serial, we can define a QuickCheck property.
    The problem we're facing though, is that we need to have a certain domain where a relation is valid.
    This is due to the "isSerial" function needing a domain to execute its seriality check. 

    While it is true that generally this relation serial, so there is always a connection,
    this doesn't mean the connection which can be established from one element to another is in the required domain.

    For example, if we consider the relation [(0,0),(1,1),(0,2),(1,3)].
    When we look at it, it would be serial, since every element is connected to another one.
    But if we were to look at it from the domain perspective of [9,10,11], 
    for example this relation wouldn't be serial anymore.

    If we were to test the seriality for a given domain, 
    then the following property for the domain for a given relation needs to be true.

    - The domain needs to be a list from [0, max (Rel a)].

    This is the case since the potential modulo operation for max (Rel a) could result in any value between 0 and itself.
    (Since n can be any positive integer) 

-}

-- Creates a relation from a domain, by applying a modulo of a given n value to every value of the domain.
genRelationFromDomain:: [Int] -> Int -> Rel Int
genRelationFromDomain domain n = do
                            [(y `mod` n, y) | y <- domain]

-- Checks if a domain and it's generated relation is serial for a given n value.
isModuloRelationSerial:: [Int] -> Int -> Bool
isModuloRelationSerial domain n = isSerial domain (genRelationFromDomain domain n)

-- We use a tuple for random domain and n generation.
-- Just for ease of use in the property check.
getRandomDomainAndModulo:: Gen ([Int], Int)
getRandomDomainAndModulo = do
                  -- We choose the arbitrary domain limitation size of 15 just to have good performance.
                  -- While also having good coverage.
                  limit <- choose(1,15)
                  let domain = [0..limit]
                  -- Since we are working with integers and n has to be larger than 0.
                  -- Therefore we pick a random number between 1 and the integer limit.
                  n <- choose(1,2147483647)
                  return (domain, n)

-- In this case the "randomly" (in the given restrictions) chosen domain and its corresponding relation
-- should always be serial, thus holding this property always true.
prop_isModuloRelationSerial:: ([Int], Int) -> Bool
prop_isModuloRelationSerial (domain, n) = isModuloRelationSerial domain n

-- All properties while running the QuickCheck test, hold true.
main :: IO()
main =
    do
        quickCheck $ forAll genListOfInts prop_isEndorelatorSerial
        quickCheck $ forAll uniqueIntList prop_hasNonSerialElement
        quickCheck $ forAll getRandomDomainAndModulo prop_isModuloRelationSerial