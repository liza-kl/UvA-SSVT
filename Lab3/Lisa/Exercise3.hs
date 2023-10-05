module Exercise3 where
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Debug.Trace
-- ## Task ##
-- Implement a function that calculates the minimal property subsets, 
-- given a 'function under test' and a set of properties

-- ## Deliverables ##
-- implementation, documentation of approach, indication of time spent
-- Time spent: 

-- ## Documentation of Approach ##
-- First naive thoughts :

-- We take as an argument something comparabable and generic (need to work with constaints)
-- Need to derive if a property is a subset of another property, if yes replace it (recursion maybe)
-- Use stronger / weaker functions and leave in the end only the strongest ones or equal and this is the subset (?).
-- You need to check if a prop is weaker than the other one, if yes then take that (because it then includes the stronger prop).

-- Probably the solution: 

-- Or we could execute the property and check how many mutants are killed with the property. Based on that we make a subset
-- Proposed in the FitSpec Paper "FitSpec discovers two possible minimal subsets of [...] As measured by the number of
-- killed mutants [...]". The combination of the two highest properties is the minimal subset. So we could start with one prop,
-- check how many mutants it kills, then go with 2 and up to the amount of properties given in a list (so the full set basically).
-- We could use the mutate' function for that. 
-- In the function we need to provide some mutators (for sake of ease we use the ones from MultiplicationTable.hs)

-- Probably solution number 2:
-- Iterate through the mutant and check by how many properties it is killed
-- 

-- 1st argument the fut
-- 2nd argument the properties
-- Result: Minimal subset(s)

-- calculateMinimalSubset :: Eq b => (Integer -> b) -> [b -> Integer -> Bool] -> [Int]
-- calculateMinimalSubset _ [] = [] -- if properties set is empty, there can't be a minimal set 
-- calculateMinimalSubset fut propsToTest =
--     let mutiers = [addElements, removeElements, anyList]
--     in map (\mutierToTest -> getKilledByProp propsToTest propToTest fut 10) mutiers

-- Sequential search an adding stuff 
-- First list of props 
-- Second: list of mutiers
-- Third: function under test 
-- Fourth: input for function under test 
-- Return the number of killed mutants with the given property set 

numberOfKilledMutants ::  (Eq a) => [a -> Integer -> Bool] -> [a -> Gen a] -> (Integer -> a) -> Integer -> Int
numberOfKilledMutants listOfProps listOfMutiers fut inputFut = do
    length [ (\mutier -> mutate' mutier listOfProps fut inputFut) | mutier <- listOfMutiers]



percentageOfKilledMutants :: Integral a => a -> a -> a
percentageOfKilledMutants totalnoMutants killedMutants = (totalnoMutants `div` 100) * killedMutants

main = do
    let status =  numberOfKilledMutants [prop_sumIsTriangleNumberTimesInput] [addElements] multiplicationTable 10
    trace ("Killed Mutants: " ++ show status) $ status;
    -- numberOfKilledMutants [prop_sumIsTriangleNumberTimesInput] [addElements] multiplicationTable 10


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
