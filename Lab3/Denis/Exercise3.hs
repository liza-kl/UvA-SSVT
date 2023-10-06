module Denis.Exercise3 where
import Denis.Exercise2
import Denis.Exercise1
import MultiplicationTable
import Data.List
import Mutation
import Test.QuickCheck
import System.IO.Unsafe


-- TODO WRITE UP THIS SHITTY APPROACH BASED ON THE VERY FORMAL DEFINITION...
killsNoMutants:: Integer -> [([Integer] -> Gen [Integer])] -> ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> Bool
killsNoMutants amount mutants property func = (countSurvivors amount mutants [property] func) == amount

generateMutationLists:: Integer -> [([Integer] -> Gen [Integer])] -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> [[Bool]]
generateMutationLists amount mutants [] func = []
generateMutationLists amount mutants (x:xs) func = do
                                                        return (generate (mutate' mutator properties func getRandomNumber))


getRandomNumber:: Integer
getRandomNumber = unsafePerformIO(generate (choose(1,1000)))

{- 

    Alternative approach:
    The minimal property subset consists of two given properties.

    1) If a property kills no mutants, then it isn't in the MSP (minimum property subset)
    2) If a property kills the same or a subset of the same mutants than another property, it is redundant, thus also not in the MPS.

    Therefore we could structuralize it in the following way.
        - We create a function which checks if for a given property of the initial property set, the mutate' function only returns "True" values. 
          In that case, all mutants survived, no mutant was killed and property 1 holds. So we can remove that property from the given set.
        - We then go through the rest of the properties and remove all properties whose mutation survival/kill result list is equal or a subset of another property's mutation list.
          In this case we can also remove the property, which is equal or weaker to another property.

    Then both properties hold, and the MPS is created from the initial property set.

    Other concept:
    We could apply a "search" or "optimization" algorithm approach and use one of the algorithms present there.
    The goal of such a search algorithm would be to optimize for the fewest amount of properties, which still hold the same result from the mutate' function as with the original property set.
    TODO Example of Search Algorithm in place??? But that could be a concept as well...

-}
