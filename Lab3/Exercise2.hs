module Exercise2 where
import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Debug.Trace
import Denis.Exercise1 (shuffleList)

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

countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> ([Integer] -> Gen [Integer]) -> (Integer -> [Integer]) -> Gen Integer
countSurvivors numberOfMutants props mutators fut =
    fmap (toInteger . length . filter id) <$> sequence $ generateListOfSurvivedMutants numberOfMutants mutators props fut


-- Tests, if the provided mutant survives a property 
-- 1st argument: number of mutants  
-- 2nd argument: property 
-- 3rd argument: function under test 
-- 4th argument, if the mutant survived

generateListOfSurvivedMutants
    :: Integer
    -> ([Integer] -> Gen [Integer])
    -> [[Integer] -> Integer -> Bool]
    -> (Integer -> [Integer])
    -> [Gen Bool]
generateListOfSurvivedMutants 0 mutator properties fut = [] -- If the list is empty, there are no 
generateListOfSurvivedMutants mutantNo mutator properties fut = do
    hasMutantSurvivedAllProps mutator properties fut 20 -- inputNumber can be anything
        : generateListOfSurvivedMutants (mutantNo - 1) mutator properties fut

-- Returns all true surviving mutations
hasMutantSurvivedAllProps
    :: Eq a
    => (a -> Gen a)
    -> [a -> Integer -> Bool]
    -> (Integer -> a)
    -> Integer
    -> Gen Bool
hasMutantSurvivedAllProps mutator props fut inputNumber = do
    mutatedValue <- mutate' mutator props fut inputNumber
    return (and mutatedValue)


-- ## Considerung the relation between properties and mutations ##

-- If we use our custom mutator "shuffleList" all mutants are going to survive, 
-- because this property only checks for 10 elements (which are generated due to the [1..10] 
-- list generation in the multiplicationTable function)
survivedMutantsShuffle :: IO Integer
survivedMutantsShuffle =
    generate $ countSurvivors 4000 [prop_tenElements] shuffleList multiplicationTable

-- To check for the shuffle, we need to add the "prop_linear" which is going to 
-- check if the difference between consecutive elements is the input. At this point we have 0 survivors.
killMutantsShuffle :: IO Integer
killMutantsShuffle =
    generate $ countSurvivors 4000 [prop_linear] shuffleList multiplicationTable

-- To check for the shuffle, we need to add the "prop_linear" which is going to 
-- check if the difference between consecutive elements is the input. At this point we have 0 survivors.



-- ## TRASH

-- generateListOfSurvivedMutants :: Integer -> [[Integer] -> Integer -> Bool] -> [Integer] -> Gen [Integer]-> (Integer -> [Integer]) -> [Gen Bool]
-- generateListOfSurvivedMutants 0 _ _ _ = [] -- if zero mutants are provided return an empty list 
-- -- generateListOfSurvivedMutants numberOfMutants listOfProps listOfMutators fut = map (\mutator -> hasMutantSurvivedAllProps listOfProps mutator fut) listOfMutators
-- generateListOfSurvivedMutants mutantNo properties mutator fut = do
--     hasMutantSurvivedAllProps properties  mutator fut 
--         : generateListOfSurvivedMutants (mutantNo - 1) mutator properties fut
--     where inputNumber = 20 -- This can be amended to whatever random value
--                            -- one wants to try generate mult table (fut) for


-- hasMutantSurvivedAllProps :: [[Integer] -> Integer -> Bool] -> ([Integer] -> Gen [Integer]) -> (Integer -> [Integer]) -> Gen Bool
-- hasMutantSurvivedAllProps propertyList mutatorToTest fut = do
--     testProps <- mutate' mutatorToTest propertyList fut 10 -- using mutate' as this takes a list of props, 
--     -- 10 could be any random number since it is an input for the multiplication table 
--     return (and testProps) -- return true if the mutant survived all given properties