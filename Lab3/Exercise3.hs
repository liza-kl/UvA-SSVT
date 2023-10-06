{-# LANGUAGE AllowAmbiguousTypes #-}
module Exercise3 where
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Debug.Trace
import Data.List
import Exercise1
import Data.Ord (comparing)
import MultiplicationTable (multiplicationTable)

-- ## Task ##
-- Implement a function that calculates the minimal property subsets, 
-- given a 'function under test' and a set of properties

-- ## Deliverables ##
-- implementation, documentation of approach, indication of time spent
-- Time spent: 10 hours (we were kinda in a loop of thoughts.)

-- Note: Input for fut is for the sake of ease through all of the exercises always
-- 5. No special meaning.

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

-- ⏸️ Solution which we implemented
-- The function "combinations" creates all subsets of the properties (subsequences)
-- We have a list of static mutators which need to be checked (getMutators) 
-- and we iterate over them to get somehow of a "matrix" (numberOfKilledMutants).

-- We end up with the number of killed mutants for every combination of properties.
-- Then we find the combination-sets for the largest numbers of killed mutants 
-- are composed of the smallest number of properties and print it.

-- ## The reason we spent to many hours (the result was then to go for a walk and get the last 
-- steps like done in 3 minutes)
-- numberOfKilledMutants contained a bug, which costed us a lot of time because we weren't and-ing the 
-- results of the checkIfMutantSurvivesTheSet function.
-- It actually does not need to return the length, or like it should, but it needs to check if the property set kills the X mutants
-- and not return the length of properties which killed the mutant. 

-- ## Helper Functions ##

-- ## genToList
-- We use Gen to List in order to convert our Generator Monad into an IO Monad which we can print
-- There should be probably a better solution for this, maybe you could have done this inline
-- but this "random" type conversions seem wrong.
genToList :: Gen [Bool] -> IO [Bool]
genToList = generate

-- ## checkIfMutantSurvivesTheSet
-- This function inspired from Exercise 2 checks for one mutator if a provided
-- set of properties kills it or not.
-- The "and-ing" in the end is necessary to get only one value in the list.
-- Without it it would return a list with the length of the property set (as it would check
-- for each property if it kills or not. The "and-ing" summarizes it for the set.)

-- One point that we can only assume at this point: The mutate' function returns a list 
-- of Bool values for _every_ property in a set (this caused us some headaches and bug hunting)
-- so we need to and the results in the end and to adhere to the function definition wrap it in a list.
-- We extracted the function from the numberOfKilledMutants for readability purposes.
checkIfMutantSurvivesTheSet :: (Eq a) => [a -> Integer -> Bool] -> (a -> Gen a) -> (Integer -> a) -> Integer -> Gen [Bool]
checkIfMutantSurvivesTheSet propSet mutator fut inputFut = do
    result <-   mutate' mutator propSet fut inputFut
    return [and result]

-- ## numberOfKilledMutants
-- Probably the "core" of this Exercise. It takes the list of mutants and a list
-- of property sets / combinations. For each mutator it is checked, if the set kills it or not (survivedMutants)
-- The result is then a Gen [Bool] with the maximum length of the amount of mutants
-- To get the correct type we are converting a second time and then applying a bunch of things.
numberOfKilledMutants ::  (Eq a) => [a -> Integer -> Bool] -> [a -> Gen a] -> (Integer -> a) -> Integer -> IO Integer
numberOfKilledMutants listOfProps listOfMutators fut inputFut = do
    let survivedMutants = map (\mutator -> checkIfMutantSurvivesTheSet listOfProps mutator fut inputFut) listOfMutators
    survivedMutants' <- mapM genToList survivedMutants
    -- The concat function creates a single list [[x],[y],[z]] -> [x,y,z], which enables us to filter the list
    -- We filter for "False" values, as those are the killed ones (see mutate')
    -- Getting the length to determine the amount of killed ones and converting it 
    -- Using function composition as it now seems more readable than the big amount of brackets
    return $ toInteger . length . filter not $ concat survivedMutants'


-- ## getSubsequences
-- Putting this subsequence function outside to not to recurse it
-- as this can lead to bugs and is unnecessary
-- Using "tail" to remove the [] as this get's generated and would throw an exception.
getSubsequences ::  [(String, [Integer] -> Integer -> Bool)] -> [[(String, [Integer] -> Integer -> Bool)]]
getSubsequences props = tail (subsequences props)

-- ## getMutators
-- Some are from Mutation.hs some are from Exercise1.hs
getMutators :: [[Integer] -> Gen [Integer]]
getMutators = [removeElements, shuffleList, anyList]

-- ## multiplicationTablePropsWithNames
-- List of tuples of (propertyName, property), because we need to save the property name somewhere
-- in order to be able it to print later. Another possibility would probably be to introduce a custom type
-- such as "NameableProp" 
-- data NameableFunction = NameableFunction {
--     name:: String,
--     propertyFunc:: [Integer] -> Integer -> Bool)
-- } and use this one.
multiplicationTablePropsWithNames :: [(String, [Integer] -> Integer -> Bool)]
multiplicationTablePropsWithNames = [("prop_tenElements", prop_tenElements),
                                    ("prop_firstElementIsInput", prop_firstElementIsInput),
                                    ("prop_sumIsTriangleNumberTimesInput", prop_sumIsTriangleNumberTimesInput),
                                    ("prop_linear", prop_linear), ("prop_moduloIsZero", prop_moduloIsZero)]

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
findMinimalPropertySubsets :: (Integer -> [Integer]) -> [(String, [Integer] -> Integer -> Bool)] -> IO ()
findMinimalPropertySubsets fut props = do
    let combinations = getSubsequences props
    results <- mapM (\combination -> do
        killed <- numberOfKilledMutants (propertiesWithoutNames combination) getMutators fut 5
        return (getCombinationPropertyNames combination, killed)
        ) (combinations)

    -- print all combinations with killed mutants
    putStrLn "All combinations:"
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

main =  do
    let fut = multiplicationTable  -- Replace with your actual function.
    let props = multiplicationTablePropsWithNames  -- Replace with your actual properties.
    findMinimalPropertySubsets fut props
-- ## CODE GRAVEYARD ##
-- Retuns number of killed mutants 
-- 1st list of props 
-- 2nd mutantToTest 
-- 3rd function unter test
-- 4rd input of fut 
-- Returns Integer (the number of killed mutants)


-- ## Not used, because too complicated.  ##
-- Wanted to to go property one by one and then creating all 
-- the combinations with this property and always compare the initial with the next one and return then
-- the minimal subset in an output list. Had some problems, e.g. how to continue with the list if the sets
-- are "equally" strong etc.

-- calculateMinimalSubset :: (Ord (IO Integer)) => (Integer -> [Integer]) -> [[[Integer] -> Integer -> Bool]] -> [Integer] -> IO [[Integer]]
-- calculateMinimalSubset _ [] outputList = return outputList -- if properties set is empty, return the outputList (base case for recursion)
-- calculateMinimalSubset fut (prop:prop':rest) outputList 
--                 | do numberOfKilledMutants prop getMutators fut 10 > numberOfKilledMutants prop' getMutators fut 10 = calculateMinimalSubset fut (concat (prop:rest))
--                 | do numberOfKilledMutants prop getMutators fut 10 < numberOfKilledMutants  prop' getMutators fut 10 = calculateMinimalSubset fut (concat (prop':rest))
--                 | do numberOfKilledMutants prop getMutators fut 10 == numberOfKilledMutants  prop' getMutators fut 10 = calculateMinimalSubset fut (concat (prop:prop':rest))
--                 | do otherwise return outputList -- if base case is reached 

