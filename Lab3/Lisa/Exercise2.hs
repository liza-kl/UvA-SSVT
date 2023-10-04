module Exercise2 where
import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import qualified Control.Arrow as surviced

-- ## Deliverables ##
-- implementation, documentation of approach, effect of using different mutators/properties, indication of time spent
-- Time spent: 

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

countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> [[Integer] -> Gen [Integer]]-> (Integer -> Gen [Integer]) -> Gen Integer
countSurvivors numberOfMutants listOfProperties listOfMutators fut =
    filter (== true) map 

-- Tests, if the provided mutant survives a property 
-- First argument: mutant
-- Second argument: property 
-- Third argument: function under test 
-- Returns True, if the mutant survived
hasMutantSurvived :: (Integer -> Gen [Integer]) -> ([Integer] -> Integer -> Bool) -> (Integer -> Gen [Integer]) ->  Bool 
hasMutantSurvived mutantToTest propertyToTest = do 
            mutate mutant property

main = do
        show (countSurvivors 4000 multiplicationTableProps [anyList] multiplicationTable)

-- ## Tests ##
