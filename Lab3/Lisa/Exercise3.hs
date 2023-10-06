{-# LANGUAGE AllowAmbiguousTypes #-}
module Exercise3 where
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Debug.Trace
import Data.List
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
-- We have a list of mutants which need to be checked and we iterate over them to get somehow of a "matrix"



-- ## Helper Functions ##

-- ## propertiesWithoutNames
-- Takes the name of the prop and the prop-function itself
-- map iterates over the list and returns the second element of a tuple 
-- -> The prop function.
propertiesWithoutNames :: [(String, a)] -> [a]
propertiesWithoutNames = map snd


-- ## genToList
-- We use Gen to List in order to convert our Generator Monad into an IO Monad which we can print
-- There should be probably a better solution for this, maybe you could have done this inline
-- but this "random" type conversions seem wrong.
genToList :: Gen [Bool] -> IO [Bool]
genToList = generate


-- ## checkIfMutantGetsKilledBySet
-- This function is used is 
checkIfMutantGetsKilledBySet :: (Eq a) => [a -> Integer -> Bool] -> (a -> Gen a) -> (Integer -> a) -> Integer -> Gen [Bool]
checkIfMutantGetsKilledBySet propSet mutant fut _ = 
    mutate' mutant propSet fut 10

-- This function contains a bug, or it's like not finished yet.
-- It actually does not need to return the length, or like it should but it needs to check if the property set kills the X mutants
-- Then it returns something. But at the moment it checks for every property in the list if it's killing it.
numberOfKilledMutants ::  (Eq a) => [a -> Integer -> Bool] -> [a -> Gen a] -> (Integer -> a) -> Integer -> IO Integer
numberOfKilledMutants listOfProps listOfMutiers fut inputFut = do
    let convert = map (\mutier -> checkIfMutantGetsKilledBySet listOfProps mutier fut 10) listOfMutiers
    convert2 <- mapM genToList convert
    return (toInteger (length (filter not (concat convert2))))


-- Basically the idea is to generate a list of all provided properties
-- This functions also provide "single" properties, up until the whole set
-- Calculate then the number of killed mutants, keep those who are "equally strong" in terms
-- of killing.


-- Putting this subsequence function outside to not to recurse it
getSubsequences ::  [(String, [Integer] -> Integer -> Bool)] -> [[(String, [Integer] -> Integer -> Bool)]]
getSubsequences props = tail (subsequences props)

-- getMutators :: [[Integer] -> Gen [Integer]]
-- getMutators = [addElements, removeElements, anyList]

-- calculateMinimalSubset :: (Ord (IO Integer)) => (Integer -> [Integer]) -> [[[Integer] -> Integer -> Bool]] -> [Integer] -> IO [[Integer]]
-- calculateMinimalSubset _ [] outputList = return outputList -- if properties set is empty, return the outputList (base case for recursion)
-- calculateMinimalSubset fut (prop:prop':rest) outputList 
--                 | do numberOfKilledMutants prop getMutators fut 10 > numberOfKilledMutants prop' getMutators fut 10 = calculateMinimalSubset fut (concat (prop:rest))
--                 | do numberOfKilledMutants prop getMutators fut 10 < numberOfKilledMutants  prop' getMutators fut 10 = calculateMinimalSubset fut (concat (prop':rest))
--                 | do numberOfKilledMutants prop getMutators fut 10 == numberOfKilledMutants  prop' getMutators fut 10 = calculateMinimalSubset fut (concat (prop:prop':rest))
--                 | do otherwise return outputList -- if base case is reached 

multiplicationTablePropsWithNames :: [(String, [Integer] -> Integer -> Bool)]
multiplicationTablePropsWithNames = [("prop_tenElements", prop_tenElements), ("prop_firstElementIsInput", prop_firstElementIsInput), ("prop_sumIsTriangleNumberTimesInput", prop_sumIsTriangleNumberTimesInput), ("prop_linear", prop_linear), ("prop_moduloIsZero", prop_moduloIsZero)]

getCombinationPropertyNames :: [(String, a)] -> [String]
getCombinationPropertyNames = map fst

--[("P1", P1), ("P2", P2)]
main = do
    -- ([("P1", P1), ("P2", P2)], 5)
    let combinations = getSubsequences multiplicationTablePropsWithNames
    results <- mapM (\combination -> do
        status <- numberOfKilledMutants (propertiesWithoutNames combination) [anyList] multiplicationTable 10
        return (getCombinationPropertyNames combination, status)
        ) combinations
    -- Print the results
    mapM_ (\(names, status) -> putStrLn $ "Combination: " ++ show names ++ ", Status: " ++ show status) results


-- ## CODE GRAVEYARD ##
-- Retuns number of killed mutants 
-- 1st list of props 
-- 2nd mutantToTest 
-- 3rd function unter test
-- 4rd input of fut 
-- Returns Integer (the number of killed mutants)

-- getKilledByProp :: Eq b => [a -> Gen a] -> (b -> Integer -> Bool) -> (Integer -> b) -> Integer -> Int
-- getKilledByProp [] _ _ _ = 0 -- If no mutiers present, no one can be killed 
-- getKilledByProp listOfMutiers propToTest functionUnderTest inputOfFut = do
--     length listOfMutiers - length ( [\ mutier
--                                     -> mutate' mutier [propToTest] functionUnderTest inputOfFut |
--                                     mutier <- listOfMutiers])
