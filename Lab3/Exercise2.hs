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

    -- starting with the inner von "filter", we filter for "False" Booleans (you could also filter for "id",
    -- but I find (== True) more readable.
    -- Then we have one list, and get the length out of it
    -- since the previous output is an "Int", we need to cast it to "Integer", another
    -- solution would be just to use the Int value in the function declaration, but Int
    -- is fixed, whereas Integer is not (well, in theory, practically it 
    -- is fixed by your hardware ressources) 

    toInteger . length . filter (== False) <$> sequence (generateListOfSurvivedMutants numberOfMutants mutators props fut)

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

-- ## Considering the relation between properties and mutations ##

-- original output: [5, 10, 15, 20, 25, 30, 35, 40, 45, 50]

-- 0 survivors
-- If we use our custom mutator "shuffleList" no mutants are going to survive, 
-- because the list elements are still 10, just shuffled
-- eg shuffleList [5, 10, 15, 20, 25, 30, 35, 40, 45, 50] = [45,10,40,35,30,25,15,20,50,5]
survivedShuffle_tenElements :: IO Integer
survivedShuffle_tenElements =
    generate $ countSurvivors 4000 [prop_tenElements] shuffleList multiplicationTable

-- 4000 survivors
-- Because the elements are shuffled, the difference between consecutive elements 
-- is not necessarily the input. So, it seems that every mutant survives.
-- eg shuffleList [5, 10, 15, 20, 25, 30, 35, 40, 45, 50] = [45,10,40,35,30,25,15,20,50,5]
survivedShuffle_linear :: IO Integer
survivedShuffle_linear =
    generate $ countSurvivors 4000 [prop_linear] shuffleList multiplicationTable

-----------------------------------------

-- 0 survivors
-- A negative list is not caught by the "prop_moduloIsZero" 
-- eg negativeList [5, 10, 15, 20, 25, 30, 35, 40, 45, 50] = [-5,-10,-15,-20,-25,-30,-35,-40,-45,-50]
survivedNegativeList_moduloIsZero :: IO Integer
survivedNegativeList_moduloIsZero =
    generate $ countSurvivors 4000 [prop_moduloIsZero] negativeList multiplicationTable

-----------------------------------------

-- 3991 survivors
-- random numbers have beed added, so the elements wont be 10 in most cases
-- eg addElements [5, 10, 15, 20, 25, 30, 35, 40, 45, 50] = [18,-6,22,19,12,-7,-3,-21,14,10,-15,-4,-8,-8,15,-6,-15,17,-20,8,-17,0,5,10,15,20,25,30,35,40,45,50]
survivedAddElements_tenElements :: IO Integer
survivedAddElements_tenElements =
    generate $ countSurvivors 4000 [prop_tenElements] addElements multiplicationTable

-- 3788 survivors
-- numbers have beed added, so the first element can be anything
-- eg addElements [5, 10, 15, 20, 25, 30, 35, 40, 45, 50] = [18,-6,22,19,12,-7,-3,-21,14,10,-15,-4,-8,-8,15,-6,-15,17,-20,8,-17,0,5,10,15,20,25,30,35,40,45,50]
survivedAddElements_firstElementIsInput :: IO Integer
survivedAddElements_firstElementIsInput =
    generate $ countSurvivors 4000 [prop_firstElementIsInput] addElements multiplicationTable

-- 3977 survivors
-- random numbers have beed added, so the sum of the output is not necessarily the input times the 10th triangle number
-- eg addElements [5, 10, 15, 20, 25, 30, 35, 40, 45, 50] = [18,-6,22,19,12,-7,-3,-21,14,10,-15,-4,-8,-8,15,-6,-15,17,-20,8,-17,0,5,10,15,20,25,30,35,40,45,50]
survivedAddElements_sumIsTriangleNumberTimesInput :: IO Integer
survivedAddElements_sumIsTriangleNumberTimesInput =
    generate $ countSurvivors 4000 [prop_sumIsTriangleNumberTimesInput] addElements multiplicationTable

-- 3997 survivors
-- random numbers have beed added, so the difference between consecutive elements is not necessarily the input
-- eg addElements [5, 10, 15, 20, 25, 30, 35, 40, 45, 50] = [18,-6,22,19,12,-7,-3,-21,14,10,-15,-4,-8,-8,15,-6,-15,17,-20,8,-17,0,5,10,15,20,25,30,35,40,45,50]
survivedAddElements_linear :: IO Integer
survivedAddElements_linear =
    generate $ countSurvivors 4000 [prop_linear] addElements multiplicationTable

-- 3991 survivors
-- random numbers have beed added, so modulo the input is not necessarily zero
-- eg addElements [5, 10, 15, 20, 25, 30, 35, 40, 45, 50] = [18,-6,22,19,12,-7,-3,-21,14,10,-15,-4,-8,-8,15,-6,-15,17,-20,8,-17,0,5,10,15,20,25,30,35,40,45,50]
survivedAddElements_moduloIsZero :: IO Integer
survivedAddElements_moduloIsZero =
    generate $ countSurvivors 4000 [prop_moduloIsZero] addElements multiplicationTable

-----------------------------------------

-- 3613 survivors
-- numbers have beed removed, so the elements wont be 10 necessarily
-- eg removeElements [5, 10, 15, 20, 25, 30, 35, 40, 45, 50] = [5, 10, 15, 20]
survivedRemoveElements_tenElements :: IO Integer
survivedRemoveElements_tenElements =
    generate $ countSurvivors 4000 [prop_tenElements] removeElements multiplicationTable

-- 0 survivors
-- numbers have beed removed from the back, so the first element will still always be the same as input
-- eg removeElements [5, 10, 15, 20, 25, 30, 35, 40, 45, 50] = [5, 10, 15, 20]
survivedRemoveElements_firstElementIsInput :: IO Integer
survivedRemoveElements_firstElementIsInput =
    generate $ countSurvivors 4000 [prop_firstElementIsInput] removeElements multiplicationTable

-- 3591 survivors
-- numbers have beed removed, so the sum of the output is not necessarily the input times the 10th triangle number
-- eg removeElements [5, 10, 15, 20, 25, 30, 35, 40, 45, 50] = [5, 10, 15, 20]
survivedRemoveElements_sumIsTriangleNumberTimesInput :: IO Integer
survivedRemoveElements_sumIsTriangleNumberTimesInput =
    generate $ countSurvivors 4000 [prop_sumIsTriangleNumberTimesInput] removeElements multiplicationTable

-- 0 survivors
-- numbers have beed removed from the back, so the difference between consecutive elements will still be the input
-- eg removeElements [5, 10, 15, 20, 25, 30, 35, 40, 45, 50] = [5, 10, 15, 20]
survivedRemoveElements_linear :: IO Integer
survivedRemoveElements_linear =
    generate $ countSurvivors 4000 [prop_linear] removeElements multiplicationTable

-- 0 survivors
-- modulo the input will still always be zero
-- eg removeElements [5, 10, 15, 20, 25, 30, 35, 40, 45, 50] = [5, 10, 15, 20]
survivedRemoveElements_moduloIsZero :: IO Integer
survivedRemoveElements_moduloIsZero =
    generate $ countSurvivors 4000 [prop_moduloIsZero] removeElements multiplicationTable

-----------------------------------------

-- 4000 survivors
-- 0 elements, never 10
survivedEmptyList_tenElements :: IO Integer
survivedEmptyList_tenElements =
    generate $ countSurvivors 4000 [prop_tenElements] emptyList multiplicationTable

-- Empty list exception
survivedEmptyList_firstElementIsInput :: IO Integer
survivedEmptyList_firstElementIsInput =
    generate $ countSurvivors 4000 [prop_firstElementIsInput] emptyList multiplicationTable

-- 4000 survivors
-- sum will always be 0
survivedEmptyList_sumIsTriangleNumberTimesInput :: IO Integer
survivedEmptyList_sumIsTriangleNumberTimesInput =
    generate $ countSurvivors 4000 [prop_sumIsTriangleNumberTimesInput] emptyList multiplicationTable

-- 0 survivors
-- always true because of 0 elements
survivedEmptyList_linear :: IO Integer
survivedEmptyList_linear =
    generate $ countSurvivors 4000 [prop_linear] emptyList multiplicationTable

-- 0 survivors
-- always true because of 0 elements
survivedEmptyList_moduloIsZero :: IO Integer
survivedEmptyList_moduloIsZero =
    generate $ countSurvivors 4000 [prop_moduloIsZero] emptyList multiplicationTable

-----------------------------------------