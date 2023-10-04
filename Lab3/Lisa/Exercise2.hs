module Exercise2 where
import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Debug.Trace

-- ## Task ##
-- Write a function countSurvivors that counts the number of survivors:
-- countSurvivors :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Integer]) -> Integer
-- Where the first argument is the number of mutants (4000 in the FitSpec example),
-- the second argument is the list of properties, 
-- and the third argument is the function under test
-- (the multiplication table function in this case).
-- The output is the number of surviving mutants (0 in the FitSpec example).
-- Document the effect of which mutations are used and which
-- properties are used on the number of survivors


-- ## Deliverables ##
-- implementation, documentation of approach, effect of using different mutators/properties, indication of time spent
-- Time spent: 180 min

-- ## Thoughts ##
-- The countSurvivors is going to be quite a specific function which is only going to work on 
-- functions which apply to the multiplicaton form. 
-- In an imperative approach I would use a count variable and increment it if a mutant survives 
    -- in functional I'd need to use recursion for that or monads. 
-- I have to iterate over the properties and for each property iterate over the mutants and then return a bool whether it
-- survived or not. If it survived, increment up the count else do nothing. 

-- ## Considerung the relation between properties and mutations 

-- ##The above-mentioned function definition is not final. Feel free to modify it, for example
-- by adding the mutations that should be used.
-- Did that by added another argument in order to document the effect of mutations better 

-- ## Notes on Function ##
-- First Argument: number of mutants
-- Second Argument: list of properties to test (the "unit" tests)
-- Third argument: List of mutators that should be applied -- in the form of [Integer] -> Gen [Integer]
-- Fourth argument is the function under test (multiplication table)

-- For sake of ease, equivalent mutants are not counted.

-- So in the end the function filters the list of mutants which survive (true values) under the list of
-- properties passed, and return total no. of mutants surviving.

countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> [[Integer] -> Gen [Integer]]-> (Integer -> [Integer]) -> Gen Integer
countSurvivors numberOfMutants props mutators fut = do
    listOfSurvivedMutants <- sequence $ generateListOfSurvivedMutants numberOfMutants props mutators fut
    return (toInteger (length (filter id listOfSurvivedMutants)))
    -- counting all True values, we can skip "filter" 
    --  with the <$> functor and the True value
    -- because the filter function looks for "True"

-- Tests, if the provided mutant survives a property 
-- 1st argument: number of mutants  
-- 2nd argument: property 
-- 3rd argument: function under test 
-- 4th argument, if the mutant survived

generateListOfSurvivedMutants :: Integer -> [[Integer] -> Integer -> Bool] -> [[Integer] -> Gen [Integer]]-> (Integer -> [Integer]) -> [Gen Bool]
generateListOfSurvivedMutants 0 _ _ _ = [] -- if zero mutants are provided return an empty list 
generateListOfSurvivedMutants numberOfMutants listOfProps listOfMutators fut = map (\mutator -> hasMutantSurvivedAllProps listOfProps mutator fut) listOfMutators


hasMutantSurvivedAllProps :: [[Integer] -> Integer -> Bool] -> ([Integer] -> Gen [Integer]) -> (Integer -> [Integer]) -> Gen Bool
hasMutantSurvivedAllProps propertyList mutatorToTest fut = do
    testProps <- mutate' mutatorToTest propertyList fut 10 -- using mutate' as this takes a list of props, 
    -- 10 could be any random number since it is an input for the multiplication table 
    return (and testProps) -- return true if the mutant survived all given properties

-- ## Tests ##

-- Testing survivors of MultiplicationTable.prop_tenElements
survivedMutantsAddElements :: IO Integer
survivedMutantsAddElements =
    generate $ countSurvivors 4000 multiplicationTableProps [addElements] multiplicationTable

-- Counting survivors in removeElements mutator
countSurviviorsInRemoveElements :: IO Integer
countSurviviorsInRemoveElements =
    generate $ countSurvivors 4000 multiplicationTableProps [removeElements] multiplicationTable

-- Counting survivors in removeElements mutator
countSurviviorsInAnyList :: IO Integer
countSurviviorsInAnyList =
    generate $ countSurvivors 4000 multiplicationTableProps [anyList] multiplicationTable


-- Testing survivors of MultiplicationTable.prop_firstElementIsInput
-- Testing survivors of MultiplicationTable.prop_sumIsTriangleNumberTimesInput
-- Testing survivors of MultiplicationTable.prop_linear
-- Testing survivors of MultiplicationTable.prop_moduloIsZero

