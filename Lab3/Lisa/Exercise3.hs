{-# LANGUAGE AllowAmbiguousTypes #-}
module Exercise3 where
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Debug.Trace
import Data.List
import Exercise1
import Data.Ord (comparing)

-- ## Task ##
-- Implement a function that calculates the minimal property subsets, 
-- given a 'function under test' and a set of properties

-- ## Deliverables ##
-- implementation, documentation of approach, indication of time spent
-- Time spent: 10 hours (we were kinda in a loop of thoughts.)

-- ## Documentation of Approach ##
-- ⏭️ You could skip until ⏸️ First naive thoughts :

-- We take as an argument something comparabable and generic (need to work with constaints)
-- Need to derive if a property is a subset of another property, if yes replace it (recursion maybe)
-- Use stronger / weaker functions and leave in the end only the strongest ones or equal and this is the subset (?).
-- You need to check if a prop is weaker than the other one, if yes then take that (because it then includes the stronger prop).

-- Or we could execute the property and check how many mutants are killed with the property. Based on that we make a subset
-- Proposed in the FitSpec Paper "FitSpec discovers two possible minimal subsets of [...] As measured by the number of
-- killed mutants [...]". The combination of the two highest properties is the minimal subset. So we could start with one prop,
-- check how many mutants it kills, then go with 2 and up to the amount of properties given in a list (so the full set basically).
-- We could use the mutate' function for that. 
-- In the function we need to provide some mutators (for sake of ease we use the ones from MultiplicationTable.hs)

-- ⏸️ Solution which we thought we implemented, but which is currently not bug-free:
-- The function "combinations" creates all subsets of the properties (including an empty list which should
-- be excluded in the best case to avoid exceptions when working with certain list operations).
-- We have a list of mutants which need to be checked and we iterate over them to get somehow of a "matrix".
-- We end up with the number of killed mutants for every combination of properties.
-- Then we find the combinations that for the largest number of killed mutants,
-- are composed of the smallest number of properties.


-- ## Helper Functions ##

-- ## genToList
-- We use Gen to List in order to convert our Generator Monad into an IO Monad which we can print
-- There should be probably a better solution for this, maybe you could have done this inline
-- but this "random" type conversions seem wrong.
genToList :: Gen [Bool] -> IO [Bool]
genToList = generate

-- ## checkIfMutantGetsKilledBySet
-- one mutator, with all properties 
checkIfMutantGetsKilledBySet :: (Eq a) => [a -> Integer -> Bool] -> (a -> Gen a) -> (Integer -> a) -> Integer -> Gen [Bool]
checkIfMutantGetsKilledBySet propSet mutator fut inputFut = 
    mutate' mutator propSet fut inputFut

-- This function contains a bug, or it's like not finished yet.
-- It actually does not need to return the length, or like it should but it needs to check if the property set kills the X mutants
-- Then it returns something. But at the moment it checks for every property in the list if it's killing it.
numberOfKilledMutants ::  (Eq a) => [a -> Integer -> Bool] -> [a -> Gen a] -> (Integer -> a) -> Integer -> IO Integer
numberOfKilledMutants listOfProps listOfMutators fut inputFut = do
    let convert = map (\mutator -> checkIfMutantGetsKilledBySet listOfProps mutator fut inputFut) listOfMutators
    convert2 <- mapM genToList convert
    return (toInteger (length (filter not (concat convert2))))

-- Putting this subsequence function outside to not to recurse it
getSubsequences ::  [(String, [Integer] -> Integer -> Bool)] -> [[(String, [Integer] -> Integer -> Bool)]]
getSubsequences props = tail (subsequences props)

-- list of mutators
getMutators :: [[Integer] -> Gen [Integer]]
getMutators = [removeElements]

-- list of tuples of (propertyName, property), because we need to save the property name somewhere
multiplicationTablePropsWithNames :: [(String, [Integer] -> Integer -> Bool)]
multiplicationTablePropsWithNames = [("prop_tenElements", prop_tenElements), ("prop_firstElementIsInput", prop_firstElementIsInput), ("prop_sumIsTriangleNumberTimesInput", prop_sumIsTriangleNumberTimesInput), ("prop_linear", prop_linear), ("prop_moduloIsZero", prop_moduloIsZero)]

-- ## getCombinationPropertyNames
-- Takes the name of the prop and the prop-function itself
-- map iterates over the list and returns the first element of a tuple 
-- -> The prop name.
-- given this [(propertyName, property), (propertyName2, property2)] get this [propertyName, propertyName2]
getCombinationPropertyNames :: [(String, a)] -> [String]
getCombinationPropertyNames = map fst

-- ## propertiesWithoutNames
-- Takes the name of the prop and the prop-function itself
-- map iterates over the list and returns the second element of a tuple 
-- -> The prop function.
-- given this [(propertyName, property), (propertyName2, property2)] get this [property, property2]
propertiesWithoutNames :: [(String, a)] -> [a]
propertiesWithoutNames = map snd

-- main functionality
findMinimalPropertySubsets :: IO ()
findMinimalPropertySubsets = do
    let combinations = getSubsequences multiplicationTablePropsWithNames
    results <- mapM (\combination -> do
        killed <- numberOfKilledMutants (propertiesWithoutNames combination) getMutators multiplicationTable 5
        return (getCombinationPropertyNames combination, killed)
        ) combinations
        
    -- print all combinations with killed mutants
    putStrLn "All comibinations:"
    mapM_ (\(names, killed) -> putStrLn $ "Combination: " ++ show names ++ ", Killed Mutants: " ++ show killed) results
    
    -- find the maximum number of killed mutants
    let maxKilled = maximum (map snd results)
    
    -- filter combinations with the maximum number of killed mutants
    let maxCombinations = filter (\(_, killed) -> killed == maxKilled) results
    
    -- group combinations with the same number of killed mutants
    let groupedCombinations = groupBy (\(_, killed1) (_, killed2) -> killed1 == killed2) maxCombinations
    
    -- flatten the inner lists
    let flattenedCombinations = concat groupedCombinations
    
    -- find the minimum length of property names among flattened combinations
    let minNameLength = minimumBy (comparing (length . fst)) flattenedCombinations
    
    -- filter combinations with the minimum length of property names
    let smallestNameCombinations = filter (\(names, _) -> length names == length (fst minNameLength)) flattenedCombinations
    
    -- print minimal property subsets
    putStrLn "-------------------------"
    putStrLn "Minimal property subsets:"
    mapM_ (\(names, killed) -> putStrLn $ "Combination: " ++ show names ++ ", Killed Mutants: " ++ show killed) smallestNameCombinations


-- ## CODE GRAVEYARD ##
-- Retuns number of killed mutants 
-- 1st list of props 
-- 2nd mutantToTest 
-- 3rd function unter test
-- 4rd input of fut 
-- Returns Integer (the number of killed mutants)

-- getKilledByProp :: Eq b => [a -> Gen a] -> (b -> Integer -> Bool) -> (Integer -> b) -> Integer -> Int
-- getKilledByProp [] _ _ _ = 0 -- If no mutators present, no one can be killed 
-- getKilledByProp listOfMutators propToTest functionUnderTest inputOfFut = do
--     length listOfMutators - length ( [\ mutator
--                                     -> mutate' mutator [propToTest] functionUnderTest inputOfFut |
--                                     mutator <- listOfMutators])

-- calculateMinimalSubset :: (Ord (IO Integer)) => (Integer -> [Integer]) -> [[[Integer] -> Integer -> Bool]] -> [Integer] -> IO [[Integer]]
-- calculateMinimalSubset _ [] outputList = return outputList -- if properties set is empty, return the outputList (base case for recursion)
-- calculateMinimalSubset fut (prop:prop':rest) outputList 
--                 | do numberOfKilledMutants prop getMutators fut 10 > numberOfKilledMutants prop' getMutators fut 10 = calculateMinimalSubset fut (concat (prop:rest))
--                 | do numberOfKilledMutants prop getMutators fut 10 < numberOfKilledMutants  prop' getMutators fut 10 = calculateMinimalSubset fut (concat (prop':rest))
--                 | do numberOfKilledMutants prop getMutators fut 10 == numberOfKilledMutants  prop' getMutators fut 10 = calculateMinimalSubset fut (concat (prop:prop':rest))
--                 | do otherwise return outputList -- if base case is reached 
