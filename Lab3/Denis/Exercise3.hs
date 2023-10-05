module Denis.Exercise3 where
import Denis.Exercise2
import Denis.Exercise1
import MultiplicationTable
import Data.List
import Mutation
import Test.QuickCheck
import System.IO.Unsafe

killsNoMutants:: Integer -> [([Integer] -> Gen [Integer])] -> ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> Bool
killsNoMutants amount mutants property func = (countSurvivors amount mutants [property] func) == amount

generateMutationLists:: Integer -> [([Integer] -> Gen [Integer])] -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> [[Bool]]
generateMutationLists amount mutants [] func = []
generateMutationLists amount mutants (x:xs) func = do
                                                        return (generate (mutate' mutator properties func getRandomNumber))


getRandomNumber:: Integer
getRandomNumber = unsafePerformIO(generate (choose(1,1000)))
