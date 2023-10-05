module Exercise2 where
import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Exercise1 

-- ## Deliverables ##
-- implementation, documentation of approach, effect of using different mutators/properties, indication of time spent
-- Time spent: 240 min (documentation takes time)

-- ## Documentation of Approach ##
-- In an imperative approach I would use a count variable and increment it if a mutant survives 
    -- in functional I'd need to use recursion for that.
    -- a number of mutants (modified output) is given (4000 in the FitSpec case)

-- I have to iterate over the properties and for each property iterate over the mutants and then return a bool whether it
-- survived or not. If it survived, increment up the count else do nothing. 

-- ## countSurvivors
-- First Argument: Number of mutants
-- Second Argument: Set of propertie(s) --> the list/set enables us to try out different
    -- combinations of properties (or testing them on their own).
-- Third argument: Provide one mutator which is going then to generate the number of prescribed mutants 
    -- Only one mutator, because if we take a list, we could not derive which property kills which kind of mutant. 
-- Fourth argument is the function under test
    -- In our case only the multiplication table 

-- So in the end the function filters the list of mutants which survive (true values) under the list of
-- properties passed, and return total no. of mutants of one kind (e.g. arbitraryList) surviving.

countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> ([Integer] -> Gen [Integer]) -> (Integer -> [Integer]) -> Gen Integer
countSurvivors numberOfMutants props mutators fut =
    -- What's happening in the fmap?
    -- generateListOfSurvivedMutants generated the list of survived mutants
    -- for a property set (or to be more precise a list of [Gen Bool] which could indicate the amount of survived mutants,
    -- as we haven't filtered the list yet)

    -- Now we need to use the sequence function to convert
    -- the list of monads [Gen Bool] into one Monad of the type Gen [Bool]
    -- with the help of the functor fmap in the infix notation <$> (a normal map function would here not work since 
    -- we are in a monadic context and applying 3 functions in the function composition.

    -- starting with the inner von "filter", we filter for "True" Booleans (you could also filter for "id",
    -- but I find (== True) more readable.
    -- Then we have one list, and get the length out of it
    -- since the previous output is an "Int", we need to cast it to "Integer", another
    -- solution would be just to use the Int value in the function declaration, but Int
    -- is fixed, whereas Integer is not (well, in theory, practically it 
    -- is fixed by your hardware ressources) 

    toInteger . length . filter (== True) <$> sequence (generateListOfSurvivedMutants numberOfMutants mutators props fut)

-- ## generateListOfSurvivedMutants

generateListOfSurvivedMutants
    :: Integer -- Number of mutants to generate 
    -> ([Integer] -> Gen [Integer]) -- The mutator which generates the random output lists 
    -> [[Integer] -> Integer -> Bool] -- A set of properties (can be combined as stated above)
    -> (Integer -> [Integer]) -- The function under test 
    -> [Gen Bool] -- Returns a list of Booleans (Survivors to the property set)
    -- based on the "outsourced" function hasMutantSurvivedAllProps
    -- If there are no mutants that need to be generated,
    -- it makes to sense to continue generate survivors
generateListOfSurvivedMutants 0 mutator properties fut = []
generateListOfSurvivedMutants mutantNo mutator properties fut = do
    -- mutator, properties, fut are given from the countSurvivors function 
    -- (as they are not chaging, no need in putting them into recursion)
    -- using the list constructor ":", we are calling the function 
    -- "hasMutantSurvivedAllProps" which checks if a mutant has survived the given
    -- prop set and insert the result to that list (either True (survived) / False (killed)) 
    -- after the ":" we append the result of the recursion (the base case is at 
    -- some point reached since we are decrementing the mutants to generate
    -- at some point. If everything works correctly the list should then have 
    -- the length == number of mutants (which could be a QuickCheck property if )
    -- we needed to write tests
    -- the last argument of hasMutantSurvivedAllProps is the input of
    -- function under test. For the sake of ease I used a static value (but you could
    -- generate a random number with RandomRIO or some Generator 
    -- in the multiplicationTable function this input determines the multiply factor
    hasMutantSurvivedAllProps mutator properties fut 5
        : generateListOfSurvivedMutants (mutantNo - 1) mutator properties fut

-- ## hasMutantSurvivedAllProps
hasMutantSurvivedAllProps ::
    ([Integer] -> Gen [Integer])
    -- The set of properties to check
    -> [[Integer] -> Integer -> Bool]
    -- The function under test 
    -> (Integer -> [Integer])
    -- The input for the function under test
    -> Integer
    -- Due to the fact that we are in a monadic context (?) we are not simply
    -- returning a "Bool"
    -> Gen Bool
hasMutantSurvivedAllProps mutator props fut inputNumber = do
    -- Using the mutate' function from the Mutation module as this has the advantage
    -- over the mutate function that it can take a list of properties, instead of only one
    -- this is better because you don't have to deal then with lists of [Gen Maybe (Bool)] 
    mutatedValue <- mutate' mutator props fut inputNumber
    -- "return" wraps then the Bool into the needed type "Gen Bool"
    return (and mutatedValue) -- If all survived (the "and-ing") we are going to return true.

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

-- A negative list is not catched by the "prop_moduloIsZero" 
surviveNegList :: IO Integer
surviveNegList =
    generate $ countSurvivors 4000 [prop_moduloIsZero] negativeList multiplicationTable

